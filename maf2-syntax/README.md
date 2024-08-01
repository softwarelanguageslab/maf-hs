# Syntax

This library provides parsers and preprocessing passes for the languages supported in MAF.

It exposes support for Scheme and Python syntax. The Python support is built on top of `language-python` and provides a minimal version that is easier to implement analyses for.

## Usage

The library can be used in a standalone-fashion (i.e. without `maf2-hs`) by adding it as an `extra-deps` in your `stack.yaml` (assuming a Stack-based project) file, as follows:

```yaml
extra-deps:
    - git: https://github.com/softwarelanguageslab/maf-hs
      commit: XXX
      subdirs:
        - maf2-syntax/
```

or alternatively if you are using Cabal only (in your cabal.project)

```yaml
source-repository-package
   type: git
   location: https://github.com/softwarelanguageslab/maf-hs.git
   tag: LATEST_COMMIT_HASH
   subdir: maf2-syntax
```

and then, add it as a dependencies to your `package.yaml` (or equivalent entry in your `projectname.cabal` file):

```yaml
dependencies:
    - maf2-syntax
```

