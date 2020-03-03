## Note that initially tried to create this using datamodelr
## but that creates nodes of plaintext type and the width isn't fixed by
## width arg and trying to change it.
## The code below is the result of capturing the dot_code before the
## call to DiagrammeR::grViz at
## https://github.com/bergant/datamodelr/blob/68ea3646080f8c8aca875acafb017b1ae3a53c2f/R/graph.R#L317
## and then hand modifying the labels and alignment to give us something a bit
## nicer to look at

## If you want to regenerate the images from the script code from the package
## root run
## Rscript scripts/cycle_example.R

graph <- DiagrammeR::grViz(
  "#data_model
digraph {
graph [rankdir=LR tooltip=\"Data Model\" ]

node [margin=0 fontcolor = \"#444444\" ]

edge [color = \"#555555\", arrowsize = 1, ]


 'A' [label = <<TABLE ALIGN=\"LEFT\" BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" COLOR=\"#555555\">
    <TR>
      <TD COLSPAN=\"1\" BGCOLOR=\"#EFEBDD\" BORDER=\"0\"><FONT COLOR=\"#000000\">     A     </FONT></TD>
    </TR>
    <TR>
      <TD ALIGN=\"CENTER\" BGCOLOR=\"#FFFFFF\" PORT=\"id\"><U>id</U></TD>
    </TR>
    <TR>
      <TD ALIGN=\"CENTER\" BGCOLOR=\"#FFFFFF\" PORT=\"b\">b</TD>
    </TR>
  </TABLE>>, shape = 'plaintext']

  'B' [label = <<TABLE ALIGN=\"LEFT\" BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" COLOR=\"#555555\">
    <TR>
      <TD COLSPAN=\"1\" BGCOLOR=\"#EFEBDD\" BORDER=\"0\"><FONT COLOR=\"#000000\">     B     </FONT></TD>
    </TR>
    <TR>
      <TD ALIGN=\"CENTER\" BGCOLOR=\"#FFFFFF\" PORT=\"id\"><U>id</U></TD>
    </TR>
    <TR>
      <TD ALIGN=\"CENTER\" BGCOLOR=\"#FFFFFF\" PORT=\"a\">a</TD>
    </TR>
  </TABLE>>, shape = 'plaintext']

'A':'b'->'B':'id'
'B':'a'->'A':'id'
}"
)

rsvg::rsvg_png(charToRaw(DiagrammeRsvg::export_svg(graph)),
               "vignettes/images/cycle_example.png", width = 750)
