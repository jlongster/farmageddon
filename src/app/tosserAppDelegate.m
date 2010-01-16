//
//  tosserAppDelegate.m
//  tosser
//
//  Created by James on 6/2/09.
//  Copyright Coptix, Inc 2009. All rights reserved.
//

#import "tosserAppDelegate.h"
#import "EAGLView.h"

@implementation tosserAppDelegate

@synthesize window;
@synthesize glView;

- (void)applicationDidFinishLaunching:(UIApplication *)application {

	glView.animationInterval = 1.0 / 60.0;
	[glView startAnimation];
}


- (void)applicationWillResignActive:(UIApplication *)application {
	glView.animationInterval = 1.0 / 5.0;
}


- (void)applicationDidBecomeActive:(UIApplication *)application {
	glView.animationInterval = 1.0 / 60.0;
}

- (void)dealloc {
	[window release];
	[glView release];
	[super dealloc];
}

@end
