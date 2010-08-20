//
//  tosserAppDelegate.h
//  tosser
//
//  Created by James on 6/2/09.
//  Copyright Coptix, Inc 2009. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <MessageUI/MessageUI.h>

@class EAGLView;

@interface tosserAppDelegate : NSObject <UIApplicationDelegate,
                                             MFMailComposeViewControllerDelegate> {
	IBOutlet UIWindow *window;
        IBOutlet UIViewController *rootController;
	IBOutlet EAGLView *glView;
}

@property (nonatomic, retain) UIWindow *window;
@property (nonatomic, retain) UIViewController *rootController;
@property (nonatomic, retain) EAGLView *glView;

- (void)showFeedback;

@end

