# tautologyChecker
a tautology checker as seen in ML for the working programmer by L.C. Paulson

# Compile
compile the tautologyChecker.fs file as a dll as following:

```fsharp
fsharpc --nologo -a tautologyChecker.fs
```


Then, reference the previous file using -r and include the script tautologyCheking.fs to generate a tautologyCheking.exe executable.

```fsharp
fsharpc -r tautologyChecker.dll tautologyChecking.fsx
```

# Run
```fsharp
mono tautologyCheking.exe
```

