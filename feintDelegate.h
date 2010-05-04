//
//  feintDelegate.h
//  farmageddon
//
//  Created by James Long on 4/17/10.
//  Copyright 2010 Coptix, Inc. All rights reserved.
//

#import "OpenFeint.h"
#import "EAGLView.h"

@interface feintDelegate : NSObject<OpenFeintDelegate> {
	EAGLView *glView;
}

@property (nonatomic, retain) EAGLView* glView;

- (feintDelegate*)initWithGLView:(EAGLView*)view;
- (void)dashboardWillAppear;
- (void)dashboardDidAppear;
- (void)dashboardWillDisappear;
- (void)dashboardDidDisappear;
- (void)userLoggedIn:(NSString*)userId;
- (BOOL)showCustomOpenFeintApprovalScreen;
+ (void)loadHighScores;
+ (BOOL)canReceiveCallbacksNow;
@end

@interface feintNotificationDelegate : NSObject<OFNotificationDelegate>
- (BOOL)isOpenFeintNotificationAllowed:(OFNotificationData*)notificationData;
- (void)handleDisallowedNotification:(OFNotificationData*)notificationData;
- (void)notificationWillShow:(OFNotificationData*)notificationData;
@end
