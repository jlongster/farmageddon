
#### Settings

### It's ok to always use "gsc" from the iPhoneSimulator build since,
### at this point in time, we are never using it to generate compiled
### code for the iPhone OS. We use it when targeting the iPhone OS
### only to generate C code. (Obviously, we can't run "gsc" from the
### iPhoneOS build since it's build for ARM.)

gsc=/usr/local/iphone/iPhoneSimulator3.1.3/bin/gsc

#### Main

all: simul_config src/init_.c

iphone: iphone_config src/init_.c

src/init_.c: src/init.scm src/ffi/gl.scm src/farmageddon.scm
	cd src && $(gsc) -link init.scm
### -track-scheme -debug-location 

config:
	echo '(define root "$(CURDIR)")' > src/config.scm

simul_config: config 
	echo '(define-expand-var SIMULATOR #t)' >> src/config.scm

iphone_config: config
	echo '(define-expand-var SIMULATOR #f)' >> src/config.scm

clean:
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
