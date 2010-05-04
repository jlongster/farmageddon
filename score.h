//
//  score.h
//  farmageddon
//
//  Created by James Long on 4/21/10.
//  Copyright 2010 Coptix, Inc. All rights reserved.
//

#import <UIKit/UIKit.h>


@interface HighScore : NSObject {
	int score;
	NSString *name;
}

@property (nonatomic) int score;
@property (nonatomic, retain) NSString *name;

@end
