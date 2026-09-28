# Plantas e produtos registrados na Anvisa

Projeto de pesquisa Fiocruz/EPGE. Versão organizada em 28/09/2026 para envio manual ao GitHub. Não é necessário configurar Git nem conectar esta pasta a uma conta.

## Fluxograma e ordem das etapas

O fluxograma abaixo é o mesmo do relatório: parte da construção do catálogo de plantas e distingue a busca pelo nome do produto da busca pelo princípio ativo.

![Fluxograma do projeto: fontes documentais, catálogo de plantas, enriquecimento com Flora do Brasil e busca na Anvisa por nome ou princípio ativo.](docs/imagens/fluxograma_base_dados.png)

[Ver o fluxograma original em PDF](Codigos/Report/Fig/fluxograma_base_dados.pdf).

Na versão organizada do projeto, siga esta ordem:

| Etapa | Arquivos e execução |
| --- | --- |
| 1. Catálogo de plantas | Use `Codigos/dados/plantas_busca.csv`, já curado. `preparar_nomes_plantas.R` só é necessário para preparar uma nova lista, sujeita a revisão. |
| 2A. Produtos por nome | A coleta v43 já terminou. Parta de `Codigos/dados/produtos_por_nome_anvisa.xlsx`; não execute o notebook novamente. |
| 2B. Medicamentos por princípio ativo | Execute `Codigos/coletar_medicamentos_principio_ativo.py` no servidor. Exportações e progresso ficam em `Codigos/Downloads/`. |
| 3. Análises e gráficos | Execute `analisar_produtos_por_nome.R` para 2A e `analisar_medicamentos.R` para 2B. As duas análises são independentes. |
| 4. Infográficos e mapas | Após a análise de medicamentos, execute `gerar_infograficos.R` e os mapas correspondentes. Os mapas de produtos por nome leem diretamente a planilha final de 2A. |
| 5. Relatório | Confira as figuras e atualize o texto e os totais antes de publicar uma nova versão do relatório. |

No fluxograma, “base final” representa o conjunto de resultados. Nos arquivos, as duas rotas permanecem separadas: ocorrência de um nome no produto não comprova presença da planta na composição.

## Bases definitivas e procedência

| Arquivo | Conteúdo e uso |
| --- | --- |
| `Codigos/dados/produtos_por_nome_anvisa.xlsx` | Planilha final recebida como `remake.xlsx`: 58.331 linhas em seis abas. Preservada byte a byte. É a única fonte das análises de alimentos, cosméticos e saneantes. |
| `Codigos/dados/plantas_busca.csv` | Lista curada de plantas, nomes e variantes. Entrada do coletor de medicamentos. |
| `Codigos/dados/plantas_fontes.csv` | Lista de origem usada na preparação botânica. |
| `Codigos/Downloads/` | Exportações da consulta por princípio ativo e `progresso_anvisa.csv`. Conservar juntos para retomar a coleta e reproduzir a análise. |
| `Codigos/ResultadosFinal/Bases/base_produto_planta.csv` | Base analítica consolidada de medicamentos, reconstruída a partir das exportações. |

O notebook `Codigos/coletar_produtos_por_nome_v43.ipynb` é o registro histórico da coleta **já concluída**. Não deve ser reexecutado para produzir as análises. Todas as suas células de código são idênticas às do original recebido; apenas o nome, a nota introdutória e as saídas incorporadas foram ajustados. Os caminhos originais no notebook são históricos. A versão v36 e os dois notebooks antigos de princípio ativo foram retirados da pasta ativa.

As seis abas da planilha final são: alimentos (8.371), cosméticos registrados (5.960), cosméticos regularizados/isentos (42.618), medicamentos por nome (655), saneantes (260) e produtos para saúde (467). Medicamentos encontrados **por nome do produto** não substituem os encontrados **por princípio ativo**.

## O que cada código faz

| Código em `Codigos/` | Função |
| --- | --- |
| `coletar_medicamentos_principio_ativo.py` | Coleta única, com validação de arquivos, tentativas automáticas e retomada. |
| `coletar_produtos_por_nome_v43.ipynb` | Histórico da coleta final de produtos por nome; não executar novamente. |
| `analisar_medicamentos.R` | Lê exportações, relaciona plantas e produtos, consolida registros e gera os resultados de medicamentos. |
| `graficos_medicamentos.R` | Figuras da análise de medicamentos; chamado pelo script anterior. |
| `analisar_produtos_por_nome.R` | Lê diretamente a planilha final e gera resumos e gráficos das quatro categorias. |
| `funcoes_analise.R` | Leitura, padronização em memória e funções de gráficos compartilhadas. |
| `gerar_infograficos.R` | Dois infográficos A4 a partir da base consolidada de medicamentos ativos. |
| `preparar_nomes_plantas.R` | Preparação botânica opcional. Grava `plantas_busca_geradas.csv` e `variacoes_para_revisao.csv`; não substitui a lista curada automaticamente. |

A pasta `Codigos - Mapas/` contém somente os três scripts do fluxo final:

| Ordem | Script | Função |
| --- | --- | --- |
| 1 | `preparar_empresas_medicamentos.R` | Atualiza os endereços e as coordenadas de medicamentos, reaproveita os caches e disponibiliza a malha estadual do IBGE. |
| 2 | `preparar_empresas_produtos_por_nome.R` | Identifica CNPJs e coordenadas de alimentos, cosméticos e saneantes a partir da planilha final. Não gera figuras alternativas. |
| 3 | `gerar_mapas_mercados_e_territorios.R` | Gera todos os mapas finais: gerais por mercado e territoriais das plantas de interesse. |

`Data/` contém os insumos e caches; `config/` contém as regras de correspondência; `MapasFinais/` contém as saídas vigentes. Os sete scripts anteriores e suas figuras foram retirados da pasta ativa e guardados apenas no ZIP de auditoria. Os códigos editoriais do atlas histórico em `Relatorios/` não fazem parte deste fluxo de mapas finais.

## Coletor de princípios ativos no servidor

Requer Python 3.10+ e servidor Linux ou macOS. Execute os comandos na raiz do projeto:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
python -m playwright install --with-deps chromium
python Codigos/coletar_medicamentos_principio_ativo.py --verificar
```

A instalação das bibliotecas do navegador pode exigir permissão administrativa no servidor. A verificação local não acessa a Anvisa. A lista atual contém 1.704 termos distintos, incluindo os nomes canônicos e suas variantes.

Faça primeiro uma consulta curta, em pasta separada:

```bash
python Codigos/coletar_medicamentos_principio_ativo.py --termo "Ginkgo biloba l." --saida /tmp/piloto_anvisa
```

Após conferir o resultado no servidor, execute a coleta completa:

```bash
python Codigos/coletar_medicamentos_principio_ativo.py
```

O navegador funciona sem interface por padrão. As saídas vão para `Codigos/Downloads/`. Transfira também a pasta `Downloads`, incluindo o progresso se quiser reaproveitar a coleta anterior. Os caminhos antigos do progresso são resolvidos pelo nome do arquivo na pasta atual.

Não há script separado de revisão sem match. O mesmo comando faz até três rodadas, com até três tentativas por opção, confirma resultados negativos duas vezes e retoma pendências quando executado novamente. Resultados negativos dos notebooks antigos são reconsultados. Downloads existentes e válidos são reaproveitados. Uma execução simultânea na mesma pasta é impedida por um lock.

| Código de saída | Significado |
| --- | --- |
| 0 | Termos selecionados concluídos. |
| 2 | Restaram pendências; repetir o mesmo comando retoma a coleta. |
| 3 | Acesso bloqueado, por exemplo HTTP 403/429. Resolver o acesso no servidor antes de repetir. |
| 130 | Interrupção pelo usuário; o progresso foi preservado. |

Falhas de infraestrutura ou dependências também podem encerrar a execução com outro código não zero. Consulte a mensagem de erro. O progresso é de retomada, não de atualização periódica: uma nova rodada de pesquisa em outra data deve usar outra pasta `--saida`, para não tratar resultados antigos como atuais.

**Limite da validação:** os testes locais incluem Chromium com respostas simuladas, exportação Excel, retomada, confirmação de ausência e bloqueios. O teste real nesta rede recebeu HTTP 403 da proteção da Anvisa antes da busca. Portanto, a execução completa no site real ainda precisa ser validada no servidor; não há garantia de que a rede dele será aceita ou de que o site não alterará seus endpoints.

## Análises e gráficos

Com R e as dependências do sistema para `sf`, `ragg` e Cairo disponíveis:

```bash
Rscript instalar_pacotes.R
Rscript Codigos/analisar_medicamentos.R
Rscript Codigos/analisar_produtos_por_nome.R
Rscript Codigos/gerar_infograficos.R
```

A análise de medicamentos exporta uma planilha de resultados, a base planta–produto, o CSV de empresas necessário aos mapas e 11 figuras em PNG/PDF. Use `--auditoria` apenas quando precisar dos CSVs detalhados e `--tabelas-relatorio` para as tabelas LaTeX. Os infográficos geram PNG, PDF e SVG, um PDF conjunto e uma única planilha de apoio. A geração de PDF usa Quartz no macOS e Cairo no Linux.

A análise por nome exporta **uma planilha de resumos e 12 figuras**, sem CSVs intermediários, em `Codigos/ResultadosProdutosNome/`. O recorte padrão conserva registros ativos de alimentos, cosméticos registrados, cosméticos isentos e saneantes, com ano de referência a partir de 2000 ou não informado, excluindo os termos `china` e `barata`. Total: **42.376 ocorrências** — 36.880 cosméticos isentos, 4.871 registrados, 459 alimentos e 166 saneantes. São ocorrências da busca, não necessariamente produtos únicos nem comprovação de composição botânica. `--todos-status` gera um recorte separado na subpasta `todos_status`.

Na base atual de medicamentos há 1.898 associações planta–produto e 1.370 produtos distintos. Entre os ativos, são **407 associações, 320 medicamentos distintos e 71 empresas**. A simplificação dos scripts preservou integralmente a base anterior. Esses totais poderão mudar após a nova coleta no servidor.

```bash
Rscript "Codigos - Mapas/preparar_empresas_medicamentos.R"
Rscript "Codigos - Mapas/preparar_empresas_produtos_por_nome.R"
Rscript "Codigos - Mapas/gerar_mapas_mercados_e_territorios.R"
```

As fontes públicas e as respostas geográficas são guardadas em cache. O código de produtos por nome baixa as fontes oficiais ausentes; `--consultar` permite completar endereços pela BrasilAPI e `--reutilizar` reaproveita o cruzamento apenas se a entrada não mudou. Seus principais resultados são três CSVs usados pelas etapas seguintes, uma planilha de auditoria; os mapas são gerados exclusivamente pelo terceiro script. Problemas de leitura de fontes oficiais ficam registrados separadamente, quando existem. Os mapas e infográficos de composição fixa devem ser inspecionados após uma nova coleta, sobretudo se o número de plantas mudar.

## Mapas gerais e territórios de plantio

O gerador `Codigos - Mapas/gerar_mapas_mercados_e_territorios.R` organiza a apresentação final dos mapas em dois conjuntos:

- **Gerais:** cinco mapas nacionais, separados em medicamentos, alimentos, cosméticos registrados, cosméticos isentos e saneantes. Incluem todas as empresas mapeáveis das bases ativas usadas no projeto.
- **Territoriais:** 12 recortes da lista “PLANTAS SELECIONADAS PARA O PLANTIO”, com as 23 espécies e suas 59 escolhas territoriais. O limite de interesse é hachurado: estado/DF quando a lista é estadual, município quando ela nomeia uma cidade. Cada imagem separa os cinco mercados em painéis.

```bash
Rscript "Codigos - Mapas/gerar_mapas_mercados_e_territorios.R"
```

Exemplos para visualizar diretamente no repositório: [mapa geral de medicamentos](Codigos%20-%20Mapas/MapasFinais/gerais/medicamentos.png) e [recorte das plantas do Distrito Federal](Codigos%20-%20Mapas/MapasFinais/territorios/distrito_federal.png).

As saídas ficam em `Codigos - Mapas/MapasFinais/`: 5 PNGs gerais, 12 PNGs territoriais, mapas interativos e uma única planilha `dados_dos_mapas.xlsx`. Abra `index.html` no navegador para acessar os interativos. Mantenha a pasta `interativos/bibliotecas/` junto dos HTMLs; os mapas usam arquivos locais e não precisam de um servidor de mapas. No GitHub, as imagens PNG são visualizáveis diretamente; baixe a pasta para abrir os HTMLs ou publique-a separadamente como site, se desejar.

Nos interativos, os controles de categoria permitem comparar mercados; clique em um ponto para ler **empresa, CNPJ, plantas/termos, tipo de evidência e precisão geográfica**. Empresas que compartilham coordenadas são reunidas no mesmo ponto por mercado, com todas as identificações no detalhe. Se mercados se sobrepuserem, selecione a categoria desejada no controle. Os recortes abrem sobre a região e seu entorno; ao afastar o mapa, aparecem as empresas associadas em todo o Brasil.

As imagens locais mostram o limite de interesse acrescido de 200 km em cada direção, apenas como enquadramento cartográfico. Isso não define distância de fornecimento, área de captação ou viabilidade logística. A hachura marca a área administrativa escolhida, não a localização das famílias ou lavouras, cujas coordenadas não constam da fonte.

O catálogo científico está em `Relatorios/Mapas Plantas Grupo Bioeconomia/config/especies.csv`. A configuração `Codigos - Mapas/config/nomes_produtos_plantas.csv` declara os nomes populares aceitos na busca e as exclusões para evitar confusões conhecidas. Nos medicamentos, o recorte usa os nomes científicos e sinônimos do catálogo. Nos demais mercados, a associação é um **indício por nome**, com espécie/composição não confirmadas quando o nome é popular. Não tratar esses pontos como comprovação de uso da planta ou de demanda comercial. Espécies sem correspondência permanecem na planilha, sem inferir ausência de mercado.

Antes de regenerar os mapas após uma nova coleta, atualize a análise de medicamentos, a geocodificação e o cruzamento `preparar_empresas_produtos_por_nome.R`. Esses scripts preparam as bases; o novo gerador reúne sua apresentação final. Os códigos e figuras antigos estão arquivados na auditoria; os relatórios publicados permanecem como retratos históricos.

## Relatórios e figuras publicadas

`Codigos/Report/Relatorio Fito.pdf` e as 24 imagens em `Codigos/Report/Fig/` são o retrato da versão publicada anteriormente. Foram preservados. As figuras das análises atualizadas estão nas pastas de resultados; **não foram inseridas automaticamente no relatório**, cujo texto e totais ainda precisam ser conciliados com a planilha final recebida. O `.tex` antigo não reproduz integralmente o PDF mais recente. O relatório de auditoria de 27/09 registra a correspondência das figuras publicadas.

## Testes e envio manual

A pasta `testes/`, na raiz, é opcional para a execução da coleta e das análises. Ela verifica falhas, downloads e retomada do coletor. `fixtures/medicamento_exemplo.xls` e `fixtures/anvisa_simulada.html` são insumos desses testes, não bases da pesquisa. Mantenha a pasta para conferir futuras alterações; ela não precisa ser enviada ao servidor para rodar a coleta.

```bash
python -m unittest discover -s testes -v
RUN_BROWSER_TESTS=1 python -m unittest discover -s testes -v
```

O segundo comando inclui testes locais com Chromium e não faz consultas reais à Anvisa. A primeira modalidade pula os testes de navegador.

`projeto_fiocruz_github.zip` contém uma seleção dos códigos finais, README, dependências, bases, exportações de medicamentos, dados geográficos de apoio e resultados atuais. Extraia o ZIP e envie seu conteúdo manualmente. Ele não inclui os documentos administrativos, backups de versões antigas, caches de consultas nem as fontes oficiais volumosas; estas últimas são obtidas pelos scripts quando necessárias. Inclui também os códigos e insumos dos relatórios técnicos selecionados, que preservam seus textos históricos e exigem revisão editorial antes de nova publicação; a compilação usa XeLaTeX. Os demais documentos do grupo permanecem somente na pasta original.

As versões retiradas foram guardadas em ZIPs de recuperação em `Auditoria/2026-09-28/`, fora do pacote de publicação. O inventário de substituições e a verificação de preservação do v43 também estão nessa pasta.

A antiga pasta `downloads_anvisa` foi retirada do projeto ativo: suas 133 exportações não são referenciadas pelo progresso nem usadas na análise atual. A cópia de recuperação está em `Auditoria/2026-09-28/downloads_anvisa_historico.zip`. A única pasta operacional de exportações é `Codigos/Downloads/`.
