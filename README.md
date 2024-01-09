# cl-ps

The tools you need when solving programming problems with Common Lisp

## (Not very) Quickstart

### Downloading cl-ps system

```
git clone https://github.com/murry2018/cl-ps.git ~/common-lisp/ps
```

### Ready to roll out!

**CAUTION** Because this project is not a quicklisp project, before
loading this system you should have downloaded the system already. See
previous section.

```
(ql:quicklisp :ps)
(ps:init)
(in-package :ps-user)  ;; optional
```


## Components

- `cmu-infix` facilities: See <https://github.com/quil-lang/cmu-infix>
- `cl-interpol` reader macros: See <https://edicl.github.io/cl-interpol/>
- `rutils` reader macros: See <https://github.com/vseloved/rutils>
- `(setf (scanf ...))` facility: Originally written by Christophe Rhodes (csr21@cam.ac.uk)

### The `ps-user` system

The `ps-user` system gives you access to all the features provided by
this package, as well as those in the `rutils` package. Since it is
unusual to create new packages when solving problems, you can simply
do what you want to do within the `ps-user` system.

### Example

```
CL-USER> (ql:quickload :ps)
CL-USER> (in-package :ps-user)
PS-USER> (init)
PS-USER> #i(sqrt(3^^2 + 4^^2))    ;; cmu-infix facility
5.0
PS-USER> (mapcar ^(expt % 2)      ;; rutils facility
                 '(1 2 3 4 5))
(1 4 9 16 25)
PS-USER> (let ((name 0) (id 0))
	   (setf (scanf "~d ~a" id name) "10019 Morris") ;; setf-scanf
	   #?"<Name: ${name}; ID: ${id}>")  ;; cl-interpol facility
"<Name: Morris; ID: 10019>"
```