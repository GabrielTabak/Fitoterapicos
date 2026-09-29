"""Coleta medicamentos por princípio ativo, com retomada e retentativas automáticas."""

import argparse
import asyncio
import csv
import hashlib
import json
import os
import re
import sys
import unicodedata
import zipfile
from datetime import datetime
from pathlib import Path
from urllib.parse import parse_qs, urlencode, urlsplit, urlunsplit

BASE_DIR = Path(__file__).resolve().parent
ARQUIVO_ENTRADA = BASE_DIR / "dados/plantas_busca.csv"
PASTA_DOWNLOADS = BASE_DIR / "Downloads"
ARQUIVO_PROGRESSO = PASTA_DOWNLOADS / "progresso_anvisa.csv"
URL = "https://consultas.anvisa.gov.br/#/medicamentos/"
VERSAO_ESTADO = "coletor_unificado_v2"
TIMEOUT_MS = 60000
STATUS_ARQUIVO = {
    "excel_baixado",
    "arquivo_existente",
    "legado_existente",
    "arquivo_legado",
}
COLUNAS_PROGRESSO = ["data_hora", "termo", "opcao", "status", "arquivo", "detalhe"]
COLUNAS_TERMOS = [
    "Nome_popular_buscado",
    "Nome_cientifico_buscado",
    "Variacoes_nome_popular_encontradas",
    "Variacoes_nome_cientifico_encontradas",
]


class ConsultaNaoConfirmada(RuntimeError):
    """Falha de uma consulta; manter o termo pendente e seguir a fila."""


class AcessoBloqueado(RuntimeError):
    """O site recusou o acesso; não insistir nesta execução."""


def chave_texto(valor):
    return (
        re.sub(r"\s+", " ", unicodedata.normalize("NFKC", str(valor)))
        .strip()
        .casefold()
    )


def carregar_termos(caminho):
    with Path(caminho).open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f, delimiter=";")
        ausentes = set(COLUNAS_TERMOS) - set(reader.fieldnames or [])
        if ausentes:
            raise ValueError(f"Colunas ausentes: {sorted(ausentes)}")
        termos = {}
        for row in reader:
            for col in COLUNAS_TERMOS:
                for termo in re.split(r"[;/]", row[col] or ""):
                    termo = termo.strip()
                    if chave_texto(termo) not in {
                        "",
                        "na",
                        "nan",
                        "none",
                        "não encontrado",
                        "nao encontrado",
                    }:
                        termos.setdefault(chave_texto(termo), termo)
    curtos = [t for t in termos.values() if len(t) < 3]
    if curtos:
        raise ValueError(f"Termos com menos de 3 caracteres: {curtos}")
    return list(termos.values())


def nome_seguro(valor):
    texto = re.sub(r"[^\w\s.-]", "_", unicodedata.normalize("NFKC", valor))
    return (
        re.sub(r"_+", "_", re.sub(r"\s+", "_", texto)).strip("._ ")[:100] or "sem_nome"
    )


def base_nome_arquivo(termo, opcao):
    return f"{nome_seguro(termo)}__{nome_seguro(opcao)}"


def arquivo_valido(caminho):
    caminho = Path(caminho)
    try:
        with caminho.open("rb") as f:
            assinatura = f.read(8)
        if assinatura == bytes.fromhex("D0CF11E0A1B11AE1"):
            import xlrd

            book = xlrd.open_workbook(caminho, on_demand=True)
            valido = book.nsheets > 0 and book.sheet_by_index(0).nrows > 0
            book.release_resources()
            return valido
        if assinatura.startswith(b"PK"):
            with zipfile.ZipFile(caminho) as z:
                return "xl/workbook.xml" in z.namelist() and z.testzip() is None
        return False
    except (OSError, ValueError, zipfile.BadZipFile):
        return False
    except ImportError:
        raise RuntimeError(
            "Instale as dependências de requirements.txt antes da coleta."
        )
    except Exception:
        return False


def localizar_arquivo_existente(termo, opcao):
    base = base_nome_arquivo(termo, opcao)
    digest = hashlib.sha256(f"{termo}\0{opcao}".encode()).hexdigest()[:12]
    candidatos = [
        PASTA_DOWNLOADS / f"{base}__{digest}.xlsx",
        PASTA_DOWNLOADS / f"{base}.xlsx",
    ]
    candidatos += sorted(
        p
        for p in PASTA_DOWNLOADS.glob(f"{base}__*.xlsx")
        if re.fullmatch(r"[0-9]{1,4}", p.stem.removeprefix(base + "__"))
    )
    for p in candidatos:
        if arquivo_valido(p):
            return p, "arquivo_existente"
    return None, None


def registrar_progresso(termo, status, opcao="", arquivo="", detalhe=""):
    registro = dict(
        zip(
            COLUNAS_PROGRESSO,
            [
                datetime.now().astimezone().isoformat(timespec="seconds"),
                termo,
                opcao,
                status,
                str(arquivo),
                f"{VERSAO_ESTADO}: {detalhe}",
            ],
        )
    )
    novo = not ARQUIVO_PROGRESSO.exists() or ARQUIVO_PROGRESSO.stat().st_size == 0
    with ARQUIVO_PROGRESSO.open("a", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=COLUNAS_PROGRESSO)
        if novo:
            w.writeheader()
        w.writerow(registro)
        f.flush()
        os.fsync(f.fileno())


def carregar_estado():
    """Retoma sucessos válidos; reavalia negativos dos coletores antigos."""
    if not ARQUIVO_PROGRESSO.exists() or not ARQUIVO_PROGRESSO.stat().st_size:
        return set(), set()
    with ARQUIVO_PROGRESSO.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        if not set(COLUNAS_PROGRESSO).issubset(reader.fieldnames or []):
            raise ValueError("Checkpoint com colunas incompatíveis.")
        rows = list(reader)
    if any(None in r or any(v is None for v in r.values()) for r in rows):
        raise ValueError(
            "Checkpoint incompleto ou malformado; preserve o arquivo e confira a última linha."
        )
    ultimos, finais = {}, {}
    for indice, row in enumerate(rows):
        row["_indice"] = indice
        termo, opcao = chave_texto(row["termo"]), chave_texto(row["opcao"])
        if opcao:
            ultimos[termo, opcao] = row
        else:
            finais[termo] = row
    opcoes, invalidos = set(), set()
    for par, row in ultimos.items():
        status = row["status"]
        if status in STATUS_ARQUIVO:
            nome = Path(row["arquivo"].replace("\\", "/")).name
            candidatos = [
                PASTA_DOWNLOADS / nome,
                Path(row["arquivo"]),
            ]
            if nome and any(arquivo_valido(p) for p in candidatos):
                opcoes.add(par)
            else:
                invalidos.add(par[0])
        elif status == "sem_registro_pos" and row["detalhe"].startswith(VERSAO_ESTADO):
            opcoes.add(par)
        elif row["_indice"] > finais.get(par[0], {}).get("_indice", -1):
            invalidos.add(par[0])
    termos = {
        t
        for t, r in finais.items()
        if r["status"] in {"termo_concluido", "sem_registro"}
        and r["detalhe"].startswith(VERSAO_ESTADO)
        and t not in invalidos
    }
    return termos, opcoes


SEL_LUPA_PRINCIPIO = (
    "div[ng-model='filter.substancia'][url-service='substancia'] " "a[modal-anvisa]"
)
SEL_LIMPAR_PRINCIPIO = (
    "div[ng-model='filter.substancia'][url-service='substancia'] "
    "a[ng-click='limpa()']"
)
SEL_CONSULTAR = "input[type='submit'][value='Consultar']"
SEL_EXCEL = "a[ng-click*='exportarExcel'], a:has-text('Exportar para Excel')"
SEL_VOLTAR = (
    "a[ng-click='voltar()'], input[value='Voltar'], "
    "button:has-text('Voltar'), a:has-text('Voltar')"
)
SEL_NENHUM_REGISTRO_POPUP = (
    "tr[ng-if='!itens.length'] " "td:has-text('Nenhum registro encontrado')"
)
PADRAO_SEM_RESULTADO = re.compile(
    r"nenhum (registro|resultado)|não foram encontrados|nao foram encontrados",
    re.IGNORECASE,
)


async def primeiro_visivel(locator):
    for indice in range(await locator.count()):
        item = locator.nth(indice)
        try:
            if await item.is_visible():
                return item
        except Exception:
            pass
    return None


async def esperar_primeiro_visivel(page, locator, timeout_ms=10000, intervalo_ms=200):
    tentativas = max(1, timeout_ms // intervalo_ms)
    for _ in range(tentativas):
        item = await primeiro_visivel(locator)
        if item is not None:
            return item
        await page.wait_for_timeout(intervalo_ms)
    return None


async def fechar_todos_modais(page):
    for _ in range(5):
        modais = page.locator("div.modal:visible")
        if await modais.count() == 0:
            return

        modal = modais.last
        botoes = modal.locator(
            "button.close, a[ng-click='cancel()'], button[ng-click='cancel()'], "
            "a:has-text('Cancelar'), button:has-text('Cancelar'), "
            "a:has-text('Fechar'), button:has-text('Fechar')"
        )
        botao = await primeiro_visivel(botoes)
        try:
            if botao is not None:
                await botao.click(force=True, timeout=2000)
            else:
                await page.keyboard.press("Escape")
            await page.wait_for_timeout(200)
        except Exception:
            await page.keyboard.press("Escape")
            await page.wait_for_timeout(200)


async def esta_na_busca_avancada(page):
    try:
        lupa = page.locator(SEL_LUPA_PRINCIPIO).first
        return await lupa.is_visible(timeout=1200)
    except Exception:
        return False


async def preparar_pagina(page):
    await fechar_todos_modais(page)
    if await esta_na_busca_avancada(page):
        await limpar_principio_ativo(page)
        return
    response = await page.goto(URL, wait_until="domcontentloaded", timeout=TIMEOUT_MS)
    if response is not None and response.status in {403, 429}:
        raise AcessoBloqueado(
            f"A sessão do Chromium recebeu HTTP {response.status} ao abrir a Anvisa. "
            "Isso não determina se o acesso funciona em outro navegador. "
            "Use --mostrar-navegador para acompanhar a tentativa local."
        )
    # A página pode abrir diretamente nos critérios avançados.
    controles = page.locator(
        f"{SEL_LUPA_PRINCIPIO}, input[value='Busca Avançada']"
    )
    controle = await esperar_primeiro_visivel(page, controles, timeout_ms=TIMEOUT_MS)
    if controle is None:
        raise RuntimeError("Página sem Princípio Ativo nem botão Busca Avançada visíveis.")
    if not await esta_na_busca_avancada(page):
        await page.locator("input[value='Busca Avançada']").first.click()
        await page.locator(SEL_LUPA_PRINCIPIO).first.wait_for(
            state="visible", timeout=TIMEOUT_MS
        )
    await limpar_principio_ativo(page)


async def garantir_tela_busca(page):
    await fechar_todos_modais(page)
    if await esta_na_busca_avancada(page):
        return

    botao_voltar = await primeiro_visivel(page.locator(SEL_VOLTAR))
    if botao_voltar is not None:
        try:
            await botao_voltar.click(force=True, timeout=5000)
            await page.wait_for_timeout(500)
            if await esta_na_busca_avancada(page):
                return
        except Exception:
            pass

    botao_avancada = page.locator("input[value='Busca Avançada']").first
    try:
        if await botao_avancada.is_visible(timeout=2000):
            await botao_avancada.click(force=True)
            await page.locator(SEL_LUPA_PRINCIPIO).first.wait_for(
                state="visible", timeout=10000
            )
            return
    except Exception:
        pass

    await preparar_pagina(page)


async def limpar_principio_ativo(page):
    if not await esta_na_busca_avancada(page):
        return
    botao = await primeiro_visivel(page.locator(SEL_LIMPAR_PRINCIPIO))
    if botao is not None:
        try:
            await botao.click(force=True, timeout=2500)
            await page.wait_for_timeout(250)
        except Exception:
            pass


async def abrir_modal_principio(page):
    await garantir_tela_busca(page)
    await fechar_todos_modais(page)
    lupa = page.locator(SEL_LUPA_PRINCIPIO).first
    await lupa.wait_for(state="visible", timeout=15000)
    await lupa.click(force=True, timeout=10000)
    modal = page.locator("div.modal:visible").last
    await modal.wait_for(state="visible", timeout=10000)
    return modal


async def texto_opcao_linha(linha):
    link = linha.locator("a[ng-click*='seleciona']").first
    if await link.count() > 0:
        texto_link = re.sub(r"\s+", " ", (await link.inner_text()).strip())
        if texto_link:
            return texto_link

    textos = []
    celulas = linha.locator("td")
    for indice in range(await celulas.count()):
        texto = re.sub(r"\s+", " ", (await celulas.nth(indice).inner_text()).strip())
        if texto and chave_texto(texto) not in {"selecionar", "selecione"}:
            textos.append(texto)
    return textos[-1] if textos else ""


async def existe_mensagem_sem_resultado(page):
    mensagens = page.get_by_text(PADRAO_SEM_RESULTADO)
    return await primeiro_visivel(mensagens) is not None


def resposta_parece_excel(response):
    headers = {str(k).lower(): str(v).lower() for k, v in response.headers.items()}
    content_type = headers.get("content-type", "")
    disposition = headers.get("content-disposition", "")
    return any(
        [
            "spreadsheet" in content_type,
            "application/vnd.ms-excel" in content_type,
            "application/octet-stream" in content_type and "attachment" in disposition,
            ".xlsx" in disposition,
            ".xls" in disposition,
        ]
    )


def resposta_do_termo(response, termo):
    url = urlsplit(response.url)
    return "substancia" in url.path.lower() and any(
        chave_texto(v) == chave_texto(termo)
        for valores in parse_qs(url.query).values()
        for v in valores
    )


def ler_pagina_opcoes(payload):
    if not isinstance(payload, dict) or not isinstance(payload.get("content"), list):
        raise RuntimeError(
            "Resposta de substâncias fora do formato esperado; não é resultado vazio."
        )
    content = payload["content"]
    total = payload.get("totalElements")
    paginas = payload.get("totalPages")
    if (
        not isinstance(total, int)
        or not isinstance(paginas, int)
        or total < 0
        or paginas < 0
    ):
        raise RuntimeError("Resposta sem contagem/paginação válida.")
    if (not content and total > 0) or (content and (total == 0 or paginas == 0)):
        raise RuntimeError("Contagem de substâncias incompatível com os resultados.")
    nomes = []
    for item in content:
        if (
            not isinstance(item, dict)
            or not isinstance(item.get("nome"), str)
            or not item["nome"].strip()
        ):
            raise RuntimeError("Substância sem nome; consulta deixada pendente.")
        nomes.append(item["nome"].strip())
    return nomes, total, paginas


async def pesquisar_modal(page, termo):
    modal = await abrir_modal_principio(page)
    campo = modal.locator("input[ng-model='filter.nome']").first
    await campo.fill(termo)
    if (await campo.input_value()).strip() != termo:
        raise RuntimeError("O pop-up não manteve o termo digitado.")
    async with page.expect_response(
        lambda r: resposta_do_termo(r, termo), timeout=TIMEOUT_MS
    ) as evento:
        await modal.locator("input[type='submit'][value='Pesquisar']").first.click()
    response = await evento.value
    if response.status == 403:
        try:
            erro = await response.json()
        except Exception:
            erro = {}
        if isinstance(erro, dict) and erro.get("codigo") == "turnstile_invalido":
            raise AcessoBloqueado(
                "A Anvisa não validou a sessão do navegador (Cloudflare Turnstile). "
                "Esta consulta NÃO confirma ausência de registros. "
                "Progresso preservado; nenhuma ausência será gravada. "
                "Verifique a validação de acesso no navegador antes de retomar."
            )
        raise AcessoBloqueado(
            "A consulta recebeu HTTP 403; não é confirmação de ausência. "
            "Progresso preservado; verifique o acesso antes de retomar."
        )
    if response.status == 429:
        raise AcessoBloqueado("Limite de consultas atingido (HTTP 429). Retome mais tarde.")
    if response.status != 200:
        raise RuntimeError(f"Busca de substâncias retornou HTTP {response.status}.")
    payload = await response.json()
    nomes, total, paginas = ler_pagina_opcoes(payload)
    return modal, response.url, nomes, total, paginas


async def pesquisar_opcoes_principio(page, termo):
    modal, url, nomes, total, paginas = await pesquisar_modal(page, termo)
    partes = urlsplit(url)
    query = parse_qs(partes.query)
    primeira = int(query.get("page", ["1"])[0])
    for numero in range(primeira + 1, primeira + paginas):
        query["page"] = [str(numero)]
        proxima = urlunsplit(partes._replace(query=urlencode(query, doseq=True)))
        response = await page.request.get(
            proxima,
            timeout=TIMEOUT_MS,
            headers={"Authorization": "Guest", "Referer": URL},
        )
        if response.status in {403, 429}:
            raise AcessoBloqueado(
                f"Anvisa recusou a paginação (HTTP {response.status})."
            )
        if response.status != 200:
            raise RuntimeError(
                f"Página {numero} de substâncias retornou HTTP {response.status}."
            )
        itens, total_pagina, total_paginas = ler_pagina_opcoes(await response.json())
        if (total_pagina, total_paginas) != (total, paginas):
            raise RuntimeError("A paginação mudou durante a busca; será repetida.")
        nomes.extend(itens)
    if len(nomes) != total:
        raise RuntimeError(f"Busca incompleta: {len(nomes)} de {total} substâncias.")
    await fechar_todos_modais(page)
    return ("opcoes" if nomes else "sem_registro"), list(dict.fromkeys(nomes))


async def selecionar_principio(page, termo, opcao):
    # Pesquisar o nome completo reduz a paginação sem mudar a opção selecionada.
    modal, _, nomes, _, _ = await pesquisar_modal(page, opcao)
    if chave_texto(opcao) not in {chave_texto(n) for n in nomes}:
        raise RuntimeError(f"Opção exata não está na primeira página: {opcao!r}.")
    for _ in range(max(1, TIMEOUT_MS // 200)):
        linhas = modal.locator("tr[ng-repeat='item in itens']")
        for indice in range(await linhas.count()):
            linha = linhas.nth(indice)
            if chave_texto(await texto_opcao_linha(linha)) == chave_texto(opcao):
                await linha.locator("a[ng-click*='seleciona']").first.click()
                await modal.wait_for(state="hidden", timeout=TIMEOUT_MS)
                return
        await asyncio.sleep(0.2)
    raise RuntimeError(f"A opção não apareceu na interface: {opcao!r}.")


async def acionar_e_salvar_excel(page, botao, destino):
    temporario = destino.with_suffix(".part")
    tarefas = {
        asyncio.create_task(page.wait_for_event("download", timeout=TIMEOUT_MS)),
        asyncio.create_task(
            page.wait_for_event(
                "response", predicate=resposta_parece_excel, timeout=TIMEOUT_MS
            )
        ),
    }
    try:
        await asyncio.sleep(0)  # Registra os listeners antes do clique.
        await botao.click()
        pendentes = tarefas.copy()
        while pendentes:
            concluidas, pendentes = await asyncio.wait(
                pendentes, return_when=asyncio.FIRST_COMPLETED
            )
            for tarefa in concluidas:
                try:
                    evento = tarefa.result()
                    if hasattr(evento, "save_as"):
                        await evento.save_as(temporario)
                    else:
                        if evento.status != 200:
                            continue
                        temporario.write_bytes(await evento.body())
                    if arquivo_valido(temporario):
                        temporario.replace(destino)
                        return
                except Exception:
                    continue
        raise RuntimeError(
            "Exportação não entregou um Excel válido; opção permanece pendente."
        )
    finally:
        for tarefa in tarefas:
            if not tarefa.done():
                tarefa.cancel()
        await asyncio.gather(*tarefas, return_exceptions=True)
        temporario.unlink(missing_ok=True)


async def consultar_e_baixar(page, termo, opcao):
    consultar = await esperar_primeiro_visivel(
        page, page.locator(SEL_CONSULTAR), timeout_ms=TIMEOUT_MS
    )
    if consultar is None:
        raise RuntimeError("Botão Consultar ausente.")
    # A nova navegação evita reutilizar uma mensagem vazia da busca anterior.
    await consultar.click()
    for _ in range(max(1, TIMEOUT_MS // 250)):
        exportar = await primeiro_visivel(page.locator(SEL_EXCEL))
        if exportar is not None:
            digest = hashlib.sha256(f"{termo}\0{opcao}".encode()).hexdigest()[:12]
            destino = (
                PASTA_DOWNLOADS / f"{base_nome_arquivo(termo, opcao)}__{digest}.xlsx"
            )
            await acionar_e_salvar_excel(page, exportar, destino)
            return destino
        if await existe_mensagem_sem_resultado(page):
            return None
        await asyncio.sleep(0.25)
    raise RuntimeError(
        "Timeout sem Excel nem mensagem explícita de ausência; não é resultado vazio."
    )


async def processar_termo(page, termo, opcoes_concluidas, tentativas):
    await preparar_pagina(page)
    estado, opcoes = await pesquisar_opcoes_principio(page, termo)
    if estado == "sem_registro":
        registrar_progresso(
            termo,
            "sem_registro",
            detalhe="Resposta HTTP 200 válida com totalElements=0.",
        )
        return True
    erros = 0
    for opcao in opcoes:
        par = chave_texto(termo), chave_texto(opcao)
        if par in opcoes_concluidas:
            continue
        existente, status = localizar_arquivo_existente(termo, opcao)
        if existente:
            registrar_progresso(termo, status, opcao, existente)
            opcoes_concluidas.add(par)
            continue
        for tentativa in range(1, tentativas + 1):
            try:
                await preparar_pagina(page)
                await selecionar_principio(page, termo, opcao)
                caminho = await consultar_e_baixar(page, termo, opcao)
                if caminho is None:
                    await preparar_pagina(page)
                    await selecionar_principio(page, termo, opcao)
                    caminho = await consultar_e_baixar(page, termo, opcao)
                registrar_progresso(
                    termo,
                    "excel_baixado" if caminho else "sem_registro_pos",
                    opcao,
                    caminho or "",
                    "Excel validado ou ausência explícita confirmada duas vezes",
                )
                opcoes_concluidas.add(par)
                break
            except AcessoBloqueado:
                raise
            except Exception as erro:
                registrar_progresso(termo, "erro_opcao", opcao, detalhe=str(erro))
                if tentativa == tentativas:
                    erros += 1
                else:
                    await asyncio.sleep(min(30, 2**tentativa))
    if erros:
        return False
    registrar_progresso(
        termo, "termo_concluido", detalhe=f"{len(opcoes)} opções verificadas"
    )
    return True


async def executar(args):
    from playwright.async_api import async_playwright

    termos = args.termo or carregar_termos(args.entrada)
    if args.limite:
        termos = termos[: args.limite]
    concluidos, opcoes = carregar_estado()
    pendentes = [t for t in termos if chave_texto(t) not in concluidos]
    print(f"{len(termos)} termos selecionados; {len(pendentes)} pendentes.", flush=True)
    if not pendentes:
        return 0
    async with async_playwright() as p:
        modo = "com janela" if args.mostrar_navegador else "sem janela; use --mostrar-navegador para exibir"
        print(f"Iniciando Chromium ({modo}).", flush=True)
        browser = await p.chromium.launch(headless=not args.mostrar_navegador)
        try:
            for rodada in range(args.rodadas):
                restantes = []
                # Uma sessão nova por rodada recupera cookies e páginas travadas.
                context = await browser.new_context(accept_downloads=True)
                page = await context.new_page()
                try:
                    for i, termo in enumerate(pendentes, 1):
                        print(
                            f"Rodada {rodada + 1}/{args.rodadas}: {i}/{len(pendentes)} {termo}",
                            flush=True,
                        )
                        try:
                            ok = await processar_termo(
                                page, termo, opcoes, args.tentativas
                            )
                        except AcessoBloqueado as erro:
                            registrar_progresso(
                                termo, "acesso_bloqueado", detalhe=str(erro)
                            )
                            print(str(erro), flush=True)
                            if args.mostrar_navegador and sys.stdin.isatty():
                                print(
                                    "Coleta pausada; o progresso já foi salvo. "
                                    "O navegador ficará aberto para você conferir a página. "
                                    "Consultas manuais nesta pausa não serão salvas pelo coletor.",
                                    flush=True,
                                )
                                try:
                                    input("Pressione Enter no Terminal para fechar e encerrar. ")
                                except EOFError:
                                    pass
                            return 3
                        except ConsultaNaoConfirmada as erro:
                            registrar_progresso(
                                termo, "consulta_nao_confirmada", detalhe=str(erro)
                            )
                            print(f"  {erro} Seguindo para o próximo termo.", flush=True)
                            ok = False
                            await asyncio.sleep(args.pausa)
                        except Exception as erro:
                            registrar_progresso(termo, "erro_termo", detalhe=str(erro))
                            print(f"  Pendente: {erro}", flush=True)
                            ok = False
                        if not ok:
                            restantes.append(termo)
                finally:
                    await context.close()
                pendentes = restantes
                if not pendentes:
                    break
                if rodada + 1 < args.rodadas:
                    await asyncio.sleep(args.pausa)
        finally:
            await browser.close()
    print(
        f"Coleta encerrada: {len(pendentes)} termos pendentes. Progresso: {ARQUIVO_PROGRESSO}"
    )
    if pendentes:
        print(
            "Execute o mesmo comando para retomar. Não é necessário um script de revisão."
        )
    return 2 if pendentes else 0


def main():
    global PASTA_DOWNLOADS, ARQUIVO_PROGRESSO, TIMEOUT_MS
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--entrada", type=Path, default=ARQUIVO_ENTRADA)
    parser.add_argument("--saida", type=Path, default=PASTA_DOWNLOADS)
    parser.add_argument(
        "--termo", action="append", help="Teste um termo; pode repetir a opção."
    )
    parser.add_argument("--limite", type=int)
    parser.add_argument("--rodadas", type=int, default=3)
    parser.add_argument("--tentativas", type=int, default=3)
    parser.add_argument(
        "--timeout",
        type=int,
        default=60,
        help="Espera máxima por resposta, em segundos.",
    )
    parser.add_argument("--pausa", type=int, default=30)
    parser.add_argument("--mostrar-navegador", action="store_true")
    parser.add_argument(
        "--verificar",
        action="store_true",
        help="Valida entrada e progresso sem acessar o site.",
    )
    args = parser.parse_args()
    if (
        min(args.rodadas, args.tentativas, args.timeout) < 1
        or args.pausa < 0
        or (args.limite is not None and args.limite < 1)
    ):
        parser.error(
            "Rodadas, tentativas, timeout e limite devem ser positivos; pausa não pode ser negativa."
        )
    if args.termo and any(len(t.strip()) < 3 for t in args.termo):
        parser.error("Cada termo deve ter pelo menos três caracteres.")
    PASTA_DOWNLOADS = args.saida.resolve()
    ARQUIVO_PROGRESSO = PASTA_DOWNLOADS / "progresso_anvisa.csv"
    TIMEOUT_MS = args.timeout * 1000
    if args.verificar:
        termos = carregar_termos(args.entrada)
        concluidos, opcoes = carregar_estado()
        print(
            f"Entrada válida: {len(termos)} termos; {len(concluidos)} termos concluídos; {len(opcoes)} opções válidas."
        )
        return 0
    PASTA_DOWNLOADS.mkdir(parents=True, exist_ok=True)
    # Linux/macOS: libera o lock automaticamente se o processo cair.
    import fcntl

    with (PASTA_DOWNLOADS / ".coleta.lock").open("w") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            parser.error("Já existe uma coleta usando esta pasta de saída.")
        try:
            return asyncio.run(executar(args))
        except KeyboardInterrupt:
            print("Interrompido; execute o mesmo comando para retomar.")
            return 130


if __name__ == "__main__":
    sys.exit(main())
