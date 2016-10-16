# tautologyChecker
a tautology checker as seen in ML for the working programmer by L.C. Paulson

# Compile
compile tautologyChecker.fs file as a dll as following:

```fsharp
fsharpc --nologo -a tautologyChecker.fs
```


Then, reference the previous file using -r and include the script to generate a tautologyCheking exe executable.

```fsharp
fsharpc -r tautologyChecker.dll tautologyChecking.fsx
```
