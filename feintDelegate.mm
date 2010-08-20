//
//  feintDelegate.m
//  farmageddon
//
//  Created by James Long on 4/17/10.
//  Copyright 2010 Coptix, Inc. All rights reserved.
//

#import "feintDelegate.h"
#import "OpenFeint.h"
#import "OFHighScoreService.h"
#import "OFHighScore.h"
#import "OFTableSectionDescription.h"
#import "OFUser.h"
#import "score.h"

#if __cplusplus
extern "C" {
#endif
    void on_scores_loaded(NSMutableArray*);
	void load_global_scores();
#if __cplusplus
}
#endif

@implementation feintDelegate

@synthesize glView;

- (feintDelegate*)initWithGLView:(EAGLView*)view {
	if([super init]) {
		glView = view;
	}
	return self;
}

- (void)dashboardWillAppear {
}

- (void)dashboardDidAppear {
	glView.animationInterval = 1.0 / 5.0;
}

- (void)dashboardWillDisappear {
}

- (void)dashboardDidDisappear {
	glView.animationInterval = 1.0 / 60.0;
	load_global_scores();
}

- (void)userLoggedIn:(NSString*)userId {
}

- (BOOL)showCustomOpenFeintApprovalScreen {
	return NO;
}

+ (void)loadHighScores {
	if([OpenFeint hasUserApprovedFeint]) {
		[OFHighScoreService getPage:1
					 forLeaderboard:@"241623"
					 friendsOnly:NO
					 silently:YES
					 onSuccess:OFDelegate(self, @selector(_scoresDownloaded:))
					 onFailure:OFDelegate(self, @selector(_failedDownloadingScores))];
	}
	else {
		on_scores_loaded(NULL);
	}
}


+ (void)_scoresDownloaded:(OFPaginatedSeries*)page {
    NSMutableArray* feintscores = [NSMutableArray new];
	NSMutableArray* highscores = [NSMutableArray new];
	
    if ([page count] > 0) {
        if ([[page objectAtIndex:0] isKindOfClass:[OFTableSectionDescription class]]) {
            // NOTE: In the following line, we access "[page objectAtIndex:1]" to retrieve high scores from
            // the global leaderboard.  Using "[page objectAtIndex:0]" would retrieve scores just for the local player.
            // Older versions of OpenFeint did not break this out into 2 sections.
			if([page count] > 1)
				feintscores = [(OFTableSectionDescription*)[page objectAtIndex:1] page].objects;
        }
        else {
            feintscores = page.objects;
        }
    }

	for(OFHighScore *feintscore in feintscores) {
		HighScore *score = [HighScore new];
		OFUser *user = feintscore.user;
		
		score.name = user.name;
		score.score = feintscore.score;
		[highscores addObject:score];
	}
	
	on_scores_loaded(highscores);
}

+ (void)_failedDownloadingScores {
}

+ (BOOL)canReceiveCallbacksNow {
    return YES;
}

@end

// http://www.cocos2d-iphone.org/forum/topic/1551
@implementation feintNotificationDelegate
- (BOOL)isOpenFeintNotificationAllowed:(OFNotificationData*)notificationData {
    //return notificationData.notificationCategory == kNotificationCategoryLogin;
    return NO;
}

- (void)handleDisallowedNotification:(OFNotificationData*)notificationData {
}

- (void)notificationWillShow:(OFNotificationData*)notificationData {
}
@end
