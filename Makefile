
#### Settings

### It's ok to always use "gsc" from the iPhoneSimulator build since,
### at this point in time, we are never using it to generate compiled
### code for the iPhone OS. We use it when targeting the iPhone OS
### only to generate C code. (Obviously, we can't run "gsc" from the
### iPhoneOS build since it's build for ARM.)

gsc=/usr/local/iphone/iPhoneSimulator3.1.3/bin/gsc

#### Main

all: simulator

iphone: iphone_config full src/init.c src/farmageddon.c src/harness.c
	cd src && $(gsc) -link -o init_.c init.c harness.c farmageddon.c

simulator: simul_config full src/init.c src/harness.c
	cd src && $(gsc) -link -o init_.c init.c harness.c
	cd src && echo '' > farmageddon.c

src/init.c: src/init.scm src/ffi/gl.scm src/farmageddon.scm
	cd src && $(gsc) -c init.scm

src/farmageddon.c: src/farmageddon.scm
	cd src && $(gsc) -c farmageddon.scm

src/harness.c: src/harness.scm
	cd src && $(gsc) -c harness.scm

### -track-scheme -debug-location 

config:
	echo '(define root "$(CURDIR)")' > src/config.scm

simul_config: config 
	echo '(define-expand-var SIMULATOR #t)' >> src/config.scm

iphone_config: config
	echo '(define-expand-var SIMULATOR #f)' >> src/config.scm

lite:
	echo '(define-expand-var LITE #t)' >> src/config.scm

full:
	echo '(define-expand-var LITE #f)' >> src/config.scm

clean:
	rm -f src/init.c
	rm -f src/farmageddon.c
	rm -f src/init_.c

#### Loadable modules
### These are here if you change the "include" statements in
### src/init.scm to "load" statements

src/ffi/ftgl.o1: src/ffi/ftgl.scm
	rm -rf src/ffi/ftgl.o1
	cd src/ffi && $(gsc) -cc-options '-I/usr/local/iphone/iPhoneSimulator3.1.2/include -I/usr/local/iphone/iPhoneSimulator3.1.2/include/freetype2' ftgl.scm

src/ffi/gl.o1: src/ffi/gl.scm
	rm -rf src/ffi/gl.o1
	cd src/ffi && $(gsc) -debug gl.scm

src/lib/srfi-1.o1: src/lib/srfi-1.scm
	rm -rf src/lib/srfi-1.o1
	cd src/lib && $(gsc) -debug srfi-1.scm
