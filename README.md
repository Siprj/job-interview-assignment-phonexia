# Build instruction (C++ code)

Repository uses git submodules, so don't forget to fetch them:

```bash
git submodule update --init --recursive
```

Inside repository run:

```bash
(mkdir -p build && cd build && cmake ../ && make)
```

This will create binary `bin/job-interview-assignment-phonexia` relative to
repository root.

# Running the app (C++ code)

Application expect CSV file with data as argument. To use data file delivered
with repository, run:

```bash
bin/job-interview-assignment-phonexia -f data/data.csv
```

# Build instruction (haskell code)

Haskell code is using tool [stack](https://docs.haskellstack.org/en/stable/README/).
How to obtain it is written on the linked page ;).

OK now when we have stack installed, lets install sandboxed haskell compiler 
with stack and afterwards lest build haskell project.

```bash
cd haskel-version/
stack setup # installs compiler
stack build # downloads all dependences and builds the project
```

# Running the app (haskell code)

```bash
cd haskel-version/
stack exec job-interview-assignment-phonexia -- -f ../data/data.csv
```
