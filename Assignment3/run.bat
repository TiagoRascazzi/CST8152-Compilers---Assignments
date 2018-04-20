
cls

echo Running Assignment3.exe on all files
cmd /C ".\Debug\Assignment3.exe" ass3e.pls>".\generated\ass3eGenerated.out"
cmd /C ".\Debug\Assignment3.exe" ass3r.pls>".\generated\ass3rGenerated.out"
cmd /C ".\Debug\Assignment3.exe" ass3w1.pls>".\generated\ass3w1Generated.out"
cmd /C ".\Debug\Assignment3.exe" ass3w2.pls>".\generated\ass3w2Generated.out"
cmd /C ".\Debug\Assignment3.exe" ass3w3.pls>".\generated\ass3w3Generated.out"
cmd /C ".\Debug\Assignment3.exe" ass3w4.pls>".\generated\ass3w4Generated.out"

cd generated

fc /B ass3e.out ass3eGenerated.out
fc /B ass3r.out ass3rGenerated.out
fc /B ass3w1.out ass3w1Generated.out
fc /B ass3w2.out ass3w2Generated.out
fc /B ass3w3.out ass3w3Generated.out
fc /B ass3w4.out ass3w4Generated.out

