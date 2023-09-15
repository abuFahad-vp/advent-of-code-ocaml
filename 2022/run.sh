
#!/bin/bash

echo "(executable
(name $1)
(libraries unix containers str angstrom))" > dune

dune exec ./$1.exe
