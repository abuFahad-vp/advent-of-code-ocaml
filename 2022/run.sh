
#!/bin/bash

echo "(executable
(name $1)
(libraries unix containers str))" > dune

dune exec ./$1.exe
