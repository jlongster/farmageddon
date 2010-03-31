//
//  main.m
//  gambit-iphone
//
//  Created by James on 4/22/09.
//  Copyright Coptix, Inc 2009. All rights reserved.
//

#define ___VERSION 406000

#import <UIKit/UIKit.h>
#include "gambit.h"
#include "stdlib.h"

#define LINKER ____20_init__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;

int main(int argc, char *argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    
    // Taken from gambit, lib/main.c. 
    int debug_settings = ___DEBUG_SETTINGS_INITIAL;
    
    // -:d- (force repl io to be stdin/stdout since terminal isn't
    // -attached)
    debug_settings =
        (debug_settings
         & ~___DEBUG_SETTINGS_REPL_MASK)
        | (___DEBUG_SETTINGS_REPL_STDIO
           << ___DEBUG_SETTINGS_REPL_SHIFT);
    // -:da
    debug_settings =
        (debug_settings
         & ~___DEBUG_SETTINGS_UNCAUGHT_MASK)
        | (___DEBUG_SETTINGS_UNCAUGHT_ALL
           << ___DEBUG_SETTINGS_UNCAUGHT_SHIFT);
    // -:dr
    debug_settings =
        (debug_settings
         & ~___DEBUG_SETTINGS_ERROR_MASK)
        | (___DEBUG_SETTINGS_ERROR_REPL
           << ___DEBUG_SETTINGS_ERROR_SHIFT);
    // -:d2
    // debug_settings =
    //     (debug_settings & ~___DEBUG_SETTINGS_LEVEL_MASK)
    //     | (2 << ___DEBUG_SETTINGS_LEVEL_SHIFT);
    
    ___setup_params_reset (&setup_params);
    setup_params.version = ___VERSION;
    setup_params.linker = LINKER;
    setup_params.min_heap = 10000;
    setup_params.debug_settings = debug_settings;


    ___setup(&setup_params);
                                  
    int retVal = UIApplicationMain(argc, argv, nil, nil);
    
    ___cleanup();
	[pool release];
	return retVal;
}
