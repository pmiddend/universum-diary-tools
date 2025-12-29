# Universum diary tools

[![Haskell CI](https://github.com/pmiddend/universum-diary-tools/actions/workflows/haskell.yml/badge.svg)](https://github.com/pmiddend/universum-diary-tools/actions/workflows/haskell.yml)

This project contains Haskell tools to parse and output statistics for the now defunct [Universum](https://play.google.com/store/apps/details?id=ru.schustovd.diary&hl=de_CH&gl=CH) diary app.

## Running

This project is a normal cabal project, so you can just do `cabal run universum-diary-tools` and it will fetch the dependencies and run the main application. It also uses Nix, so `nix develop` will give you a development shell.

## Examples

### Converting your old diary to Markdown and then PDF

We have the `convert-to-markdown` tool for this purpose. It's easy to use: Create your backup zip file, copy it somewhere, and extract it. You end up with `data.pr` and a `Photo` directory. Then create a directory `output` (via `mkdir output`) and run:

```
cabal run universum-diary-tools -- --input-file path-to/data.pr convert-to-markdown --output-dir output/
```

You will end up with a list of files:

```
output/2022.md
output/2023.md
output/2024.md
output/2025.md
```

Either store these, along with the `Photo` directory, so:

```
output/2022.md
output/2023.md
output/2024.md
output/2025.md
output/Photo
```

which is just fine, since Markdown will survive as a format, and is human-readable enough anyways. *Or* convert it to a PDF, with a command-line executed inside the `output` directory such as:

```
for i in $(seq 2022 2025); do 
  echo "converting $i.pdf"; 
  pandoc --pdf-engine=lualatex -i "$i.md" -o "$i.pdf"; 
done
```

Note that you need `lualatex` for this to work, as this has emoji support.

### Outputting Statistics
Say you made a backup of your Universum notes. This will be a `.zip` file. Extract it, and you get a file called `data.pr` (which is really a json file). This will be the input for all programs contained here.

To plot your mood over time, averaging over 30 days (it will be a moving average) use this command:

```
cabal run -- --input-file data.pr plot-mood --output-file mood.svg --average-days 30
```

To plot the number of words typed over time, use this command:

```
cabal run -- --input-file data.pr plot-word-count --output-file word-count.svg --average-days 30
```

The output of my word count so far, for example, is:

![Word count graph](./images/word-count.svg)
