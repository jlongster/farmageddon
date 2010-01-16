//
//  tosserAppDelegate.h
//  tosser
//
//  Created by James on 6/2/09.
//  Copyright Coptix, Inc 2009. All rights reserved.
//

#import <UIKit/UIKit.h>

@class EAGLView;

@interface tosserAppDelegate : NSObject <UIApplicationDelegate> {
	IBOutlet UIWindow *window;
	IBOutlet EAGLView *glView;
}

@property (nonatomic, retain) UIWindow *window;
@property (nonatomic, retain) EAGLView *glView;

@end

