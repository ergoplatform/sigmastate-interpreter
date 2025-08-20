[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/docs/zerojoin)

The `compile.sh` script in the `.autodoc/docs/json/docs/zerojoin` folder is a shell script that automates the process of compiling a LaTeX document into a PDF. This script is particularly useful in projects that involve writing technical documents in LaTeX, as it saves time and ensures that the document is always up-to-date with the latest changes.

The script first checks if the necessary commands, `pdflatex` and `bibtex`, are installed on the system by using the `command -v` command. If either of these commands is not found, the script prints an error message and exits with a status code of 1.

Assuming both commands are found, the script then runs `pdflatex` on a file named "main". This generates an auxiliary file, which is used by `bibtex` to generate a bibliography. The script then runs `pdflatex` again to incorporate the bibliography into the document, and runs it a final time to ensure all references are properly updated.

After the PDF is generated, the script removes the auxiliary files created during the compilation process to keep the working directory clean.

This script can be used as part of a larger project that involves writing technical documents in LaTeX. By automating the compilation process, it saves time and ensures that the document is always up-to-date with the latest changes. The script can be run from the command line or integrated into a build system to automatically generate the PDF whenever the source files are updated.

Example usage:

```bash
$ ./compile.sh
```

This will compile the LaTeX document named "main.tex" in the current directory and generate a PDF named "main.pdf".
