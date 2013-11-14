# gets the input files and generates the expect files
ls | grep c | sed 's/.c//' | xargs -I% sh -c "../../../../build/default/c4 --tokenize %.c >> %.e"
