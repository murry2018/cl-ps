# cl-ps

The tools you need when solving programming problems with Common Lisp

## (Not very) Quickstart

### Downloading cl-ps system

```
git clone https://github.com/murry2018/cl-ps.git ~/common-lisp/ps
```

### loading cl-ps system

**CAUTION** Because this project is not a quicklisp project, before
loading this system you should have downloaded the system already. See
previous section.

With quicklisp:

```
(ql:quicklisp :ps)
```

Or, with ASDF:

```
(asdf:load-system :ps)
```

### Ready to roll out!

```
(named-readtables:in-readtable :ps


## Components

- `cmu-infix` facilities: See <https://github.com/quil-lang/cmu-infix>
- `cl-interpol` reader macros: See <https://edicl.github.io/cl-interpol/>
- `rutils` reader macros: See <https://github.com/vseloved/rutils>
- `(setf (scanf ...))` facility

### Example