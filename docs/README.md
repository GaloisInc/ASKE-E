# Documentation

This documentation system is powered by [mdBook](https://rust-lang.github.io/mdBook/)

It aspires to contain mostly Explanation documentation as well as Reference documentation related to the research behind ASKE-E.
For more information on types of documentation, see [here](https://documentation.divio.com/).

- Instructions for Setting up mdBook can be found [here](https://rust-lang.github.io/mdBook/cli/index.html).
  This book works with all versions of mdBook available.
- Local development can be done by running [mdBook serve](https://rust-lang.github.io/mdBook/cli/serve.html) from this directory, or by running `mdBook serve path/to/this/directory`.

Alternatively, one may use docker to run mdBook by running the command

```
docker run --rm -it -p 3000:3000 --init -w /askee/docs -v $PWD:/askee peaceiris/mdbook serve -n 0.0.0.0
```

at the root of the git repo.
(The `--init` flag is important as otherwise mdBook does not respond to signals properly).
