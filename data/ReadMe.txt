ReadMe of files in the folder "data"

# csv, xlsx
capitals.csv: coordinates of capitals. Collected by ALLuza
get_missing_dois.csv: from the list sent by O Luiz, ALLuza tried to obtain the dois of those papers whose doi was not cited in the file "papers_doi.csv"
inconsistencies.csv: ALLuza found some inconsistencies in the citation of papers (column "SR" in the file "papers_doi.csv")
inconsistencias.xlsx: the same spreadsheet as the previous one, as well a spreadsheet with R codes to replace wrong citations ("SR" column)
notas (2).txt: note from O Luiz
papers_doi.csv: list of papers with DOI (the missing DOIs were handled on "get_missing_dois.csv"
papers_list_used_31_03_21.csv: all papers used in the topic modeling
Planilha_paper_x_topicos.csv: topic modeling results, with rows=papers, cols=topics, values= probabilities
RefsModified.csv: References of all papers
terms.csv: termos dentro dos tópicos buscados pela modelagem de tópicos.
instituicoes_final.csv = instituicao dos primeiros autores de cada artigo

# folders
MPAs: shapefile of Brazilian MPAs, provided by Rafael Magris
South_America: shapefile of South America (for mapping, cropping ...)
BaseLines_2: spatial line of the economic exclusive zone of BR
HistoricalData_60s_90s = Pasta com dados geográficos de estudos publicados antes de 2000.


Nota  (email do Osmar)

Oi André,

Segue uma lista preliminar de papers incluindo o numero de DOI (quando disponível). Essa planilha se chama ‘papers_doi.csv’.

Entretanto, essa lista é da busca geral que fiz no Web of Science. Depois foi passado um filtro e muitos papers foram eliminados. Então vc precisa fazer um match com a lista de papers que foram efetivamente usada na análise que estão na planilha 'papers_list_used_31_03_21.csv’.

Abs
Osmar
