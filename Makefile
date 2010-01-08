
#### Settings

### It's ok to always use "gsc" from the iPhoneSimulator build since,
### at this point in time, we are never using it to generate compiled
### code for the iPhone OS. We use it when targeting the iPhone OS
### only to generate C code. (Obviously, we can't run "gsc" from the
### iPhoneOS build since it's build for ARM.)

gsc=/usr/local/Gambit-C/iPhoneSimulator-3.1.2/bin/gsc

#### Main

all: simul_config lib/init_.c

iphone: iphone_config lib/init_.c

lib/init_.c: lib/init.scm lib/ffi/gl.scm lib/util/srfi-1.scm lib/animattack.scm
	cd lib && $(gsc) -link init.scm

config:
	echo '(define root "$(CURDIR)")' > lib/config.scm

simul_config: config 
	echo '(define-expand-var SIMULATOR #t)' >> lib/config.scm

iphone_config: config
	echo '(define-expand-var SIMULATOR #f)' >> lib/config.scm

#### UNUSED
### The following sections are UNUSED unless you want to play around
### with more advanced ways of compilation and deployment.

#### Loadable modules
### These are here if you change the "include" statements in
### lib/init.scm to "load" statements

lib/ffi/gl.o1: lib/ffi/gl.scm
	rm -rf lib/ffi/gl.o1
	cd lib/ffi && $(gsc) -debug gl.scm

lib/util/srfi-1.o1: lib/util/srfi-1.scm
	rm -rf lib/util/srfi-1.o1
	cd lib/util && $(gsc) -debug srfi-1.scm

lib/graphics.o1: lib/graphics.scm
	rm -rf lib/graphics.o1
	cd lib && $(gsc) graphics.scm

#### Making tosser.app
### These are here for manual compilation and deployment of the app

app_name=tosser.app
exe_name=tosser
uuid=6105AA5B-02C7-4250-9D68-4789C9EE0ECF
deploy_path=~/Library/'Application Support/iPhone Simulator'/User/Applications/$(uuid)

gcc=/Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/gcc-4.2
sdk=/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator3.1.sdk
# gcc=/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin/arm-apple-darwin9-gcc-4.0.1
# sdk=/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS3.0.sdk

tosser.app: Info.plist app/main.m app/EAGLView.m lib/init_.c config
	mkdir -p $(app_name)
	cp Info.plist $(app_name)
	ibtool --errors --warnings --notices \
		--output-format human-readable-text \
		--compile $(app_name)/window.nib \
		app/window.xib

	$(gcc) -x objective-c -arch i386 -isysroot $(sdk) \
	-D__IPHONE_OS_VERSION_MIN_REQUIRED=30000 \
	-mmacosx-version-min=10.5 \
	-framework Foundation -framework UIKit \
	-framework OpenGLES -framework QuartzCore \
	-framework CoreGraphics \
	-framework OpenAL -framework AudioToolbox \
	-fvisibility=hidden -I/usr/local/include -D___LIBRARY -lgambc \
    -I/usr/local/Gambit-C/iPhoneSimulator3.1/include \
    -L/usr/local/Gambit-C/iPhoneSimulator3.1/lib \
	app/EAGLView.m app/tosserAppDelegate.m app/main.m \
    lib/init*.c \
	-o $(app_name)/$(exe_name)

	cp -r resources/* $(app_name)

deploy: tosser.app
	rm -fr $(deploy_path)/$(app_name)
	cp -r $(app_name) $(deploy_path)
