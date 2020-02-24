library(dagitty)
library(ggdag)

#make a Food Security DAG

fsDag <- dagify("wtperception" ~ "BMI" + "FI" + "Sex" + "Age" + "Race" + "Income" +"Edu",
                "FI" ~ "Income" + "Sex" + "Age" + "Race" + "Edu",
                "BMI" ~ "FI",
                "Income" ~ "Edu" + "Age" + "Sex" + "Race",
                exposure = "FI",
                outcome = "wtperception") 
ggdag(fsDag)
ggdag_adjustment_set(fsDag, shadow = T) + theme_dag()


library(DiagrammeR)
#make dz specific dag
grViz("
	digraph causal {
	
	  # Nodes
	  node [shape = plaintext]
	  A [label = 'Age']
	  S [label = 'Sex']
	  I [label = 'Income']
	  R [label = 'Race']
	  B [label = 'BMI']
	  E [label = 'Education']
	  F [label = 'Food\nInsecurity']
	  Y [label = 'Weight\nPerception']
	  
	  # Edges
	  edge [color = black,
	        arrowhead = vee]
	  rankdir = LR
	  A -> F
	  A -> Y
	  R -> Y
	  R -> F
	  R -> I
	  E -> I
	  E -> Y
	  I -> Y
	  I -> F
	  I -> B
	  E -> B
	  S -> F
	  S -> Y
	  B -> Y
	  F -> B
    F -> Y

	  
	  # Graph
	  graph [overlap = true, fontsize = 10]
	}")
####################################################################
