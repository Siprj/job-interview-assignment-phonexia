# Build instruction

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

# Running the app

Application expect CSV file with data as argument. To use data file delivered
with repository, run:

```bash
bin/job-interview-assignment-phonexia -f data/data.csv
```
