# Java Parser to PlantUML

To generate the diagrams, just use the command : ```./java-parser ./directory-name -a``` </br>
It will generate one big diagram with all the classes but if you want to have several small diagrams, just disable the ***-a*** flag
To specify the name of the diagram's file and the package's diagram you want to generate change the options at the beginning of the makefile.
By default the folder is **./src/** and the name is **output**.</br>
You must have graphviz on your computer to use the plantUML jar. To install graphviz -> ```sudo apt install graphviz```</br>