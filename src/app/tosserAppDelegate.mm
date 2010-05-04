//
//  tosserAppDelegate.m
//  tosser
//
//  Created by James on 6/2/09.
//  Copyright Coptix, Inc 2009. All rights reserved.
//

#import "tosserAppDelegate.h"
#import "EAGLView.h"
#import "OpenFeint.h"
#import "feintDelegate.h"

@implementation tosserAppDelegate

@synthesize window;
@synthesize glView;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
	NSDictionary* settings = [NSDictionary dictionaryWithObjectsAndKeys:
							  [NSNumber numberWithInt:UIInterfaceOrientationPortrait], OpenFeintSettingDashboardOrientation,
							  @"Farmageddon", OpenFeintSettingShortDisplayName, 
							  [NSNumber numberWithBool:NO], OpenFeintSettingEnablePushNotifications,
							  nil
							  ];		


	OFDelegatesContainer* delegates = [OFDelegatesContainer
									   containerWithOpenFeintDelegate: [[feintDelegate alloc] initWithGLView:glView]
									   andChallengeDelegate:nil
									   andNotificationDelegate: [feintNotificationDelegate new]];
	
	[OpenFeint initializeWithProductKey: @"wKqXg2AnuizMPYY889w"
							  andSecret: @"u9GskO6O8LBKhLLF6lMkmQbut2TiKw5J4Q6Z3TZWE"
						 andDisplayName: @"Farmageddon"
							andSettings: settings
						   andDelegates: delegates];
	
	glView.animationInterval = 1.0 / 60.0;
	[glView startAnimation];
}

- (void)applicationWillResignActive:(UIApplication *)application {
	glView.animationInterval = 1.0 / 5.0;
	[OpenFeint applicationWillResignActive];
}


- (void)applicationDidBecomeActive:(UIApplication *)application {
	glView.animationInterval = 1.0 / 60.0;
	[OpenFeint applicationDidBecomeActive];
}

- (void)applicationWillTerminate:(UIApplication *)application {
	[OpenFeint shutdown];
}

- (void)dealloc {
	[window release];
	[glView release];
	[super dealloc];
}

@end
