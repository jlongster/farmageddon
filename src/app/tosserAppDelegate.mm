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
#import "Appirater.h"

@implementation tosserAppDelegate

@synthesize window;
@synthesize rootController;
@synthesize glView;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
    NSDictionary* settings = [NSDictionary dictionaryWithObjectsAndKeys: [NSNumber numberWithInt:UIInterfaceOrientationPortrait], OpenFeintSettingDashboardOrientation,
                                           @"Farmageddon", OpenFeintSettingShortDisplayName, 
                                               [NSNumber numberWithBool:NO], OpenFeintSettingEnablePushNotifications,
                                           nil
                              ];		

    
    OFDelegatesContainer* delegates = [OFDelegatesContainer containerWithOpenFeintDelegate: [[feintDelegate alloc] initWithGLView:glView]
                                                                      andChallengeDelegate:nil
                                                                   andNotificationDelegate: [feintNotificationDelegate new]];
    
    [OpenFeint initializeWithProductKey: @"wKqXg2AnuizMPYY889w"
                              andSecret: @"u9GskO6O8LBKhLLF6lMkmQbut2TiKw5J4Q6Z3TZWE"
                         andDisplayName: @"Farmageddon"
                            andSettings: settings
                           andDelegates: delegates];

    [window addSubview:rootController.view];
    [window makeKeyAndVisible];

    glView.delegate = self;
    glView.animationInterval = 1.0 / 60.0;
    [glView startAnimation];

    [Appirater appLaunched];
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

- (void)showFeedback {
    MFMailComposeViewController* controller = [[MFMailComposeViewController alloc] init];
    controller.mailComposeDelegate = self;
    [controller setSubject:@"Farmageddon Feedback"];
    [controller setToRecipients:[NSArray arrayWithObjects:@"info@farmageddongame.com", nil]];
    [controller setMessageBody:@"" isHTML:NO]; 
    [rootController presentModalViewController:controller animated:YES];
    [controller release];
}

- (void)mailComposeController:(MFMailComposeViewController*)controller didFinishWithResult:(MFMailComposeResult)result error:(NSError*)error {
    [rootController becomeFirstResponder];
    [rootController dismissModalViewControllerAnimated:YES];
}

- (void)dealloc {
	[window release];
	[glView release];
	[super dealloc];
}

@end
