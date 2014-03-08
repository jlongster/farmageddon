//
//  EAGLView.m
//  gambit-iphone
//
//  Created by James on 4/22/09.
//  Copyright James Long 2009. All rights reserved.
//

#import <QuartzCore/QuartzCore.h>
#import <OpenGLES/EAGLDrawable.h>
#import "OpenFeint.h"
#import "OFHighScoreService.h"
#import "EAGLView.h"
#import "feintDelegate.h"
#import "tosserAppDelegate.h"

enum {
    INFO_FEEDBACK = 1,
    INFO_WEBSITE,
    INFO_TWITTER,
    INFO_DEVELOPER,
};

#define USE_DEPTH_BUFFER 1

#if __cplusplus
extern "C" {
#endif
    void register_view(UIView*);
    void init();
    void render();
    void touches_began(NSSet*, UIEvent*);
    void touches_moved(NSSet*, UIEvent*);
    void touches_ended(NSSet*, UIEvent*);
    void touches_cancelled(NSSet*, UIEvent*);
	void save_score();
#if __cplusplus
}
#endif


@interface EAGLView ()

@property (nonatomic, retain) EAGLContext *context;
@property (nonatomic, assign) NSTimer *animationTimer;

- (BOOL) createFramebuffer;
- (void) destroyFramebuffer;

@end

@implementation TextFieldDoneDelegate

- (bool)textFieldShouldReturn:(UITextField*)field {
    [field resignFirstResponder];
	save_score();
	return YES;
}

@end

@implementation EAGLView

@synthesize delegate;
@synthesize context;
@synthesize animationTimer;
@synthesize animationInterval;

// You must implement this
+ (Class)layerClass {
	return [CAEAGLLayer class];
}

//The GL view is stored in the nib file. When it's unarchived it's sent -initWithCoder:
- (id)initWithCoder:(NSCoder*)coder {
    if ((self = [super initWithCoder:coder])) {
        // Get the layer
        CAEAGLLayer *eaglLayer = (CAEAGLLayer *)self.layer;
		
        eaglLayer.opaque = YES;
        eaglLayer.drawableProperties = [NSDictionary dictionaryWithObjectsAndKeys:
                                                         [NSNumber numberWithBool:NO],
                                                     kEAGLDrawablePropertyRetainedBacking,
                                                     kEAGLColorFormatRGBA8,
                                                     kEAGLDrawablePropertyColorFormat,
                                                     nil];

        if([self isHighRes]) {
            self.contentScaleFactor = 2.0;
        }
        
        context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES1];

        if (!context || ![EAGLContext setCurrentContext:context]) {
            [self release];
            return nil;
        }

        register_view(self);
        init();
    }
    return self;
}

- (BOOL)isHighRes {
    UIScreen* mainscr = [UIScreen mainScreen];
    
    if([mainscr respondsToSelector:@selector(currentMode)]) {
        CGSize screenSize = mainscr.currentMode.size;
        
        if(screenSize.width == 640.f && screenSize.height == 960.f) {
            return YES;
        }
    }

    return NO;
}

- (void)hideInfoButton {
    infoButton.hidden = YES;
}

- (void)showInfoButton {
    infoButton.hidden = NO;
}

- (IBAction)showInfo {
    infoAlert = [[UIAlertView alloc] initWithTitle:@"Information"
                                                        message:nil
                                                       delegate:self
                                              cancelButtonTitle:@"Cancel"
                                              otherButtonTitles:nil];
    [infoAlert addButtonWithTitle:@"Feedback"];    
    [infoAlert addButtonWithTitle:@"Website"];
    [infoAlert addButtonWithTitle:@"Twitter"];
    [infoAlert addButtonWithTitle:@"Developed by James Long"];
    [infoAlert show];
}

- (void)alertView:(UIAlertView*)view clickedButtonAtIndex:(NSInteger)idx {
    UIApplication *app = [UIApplication sharedApplication];

    if(idx == INFO_FEEDBACK) {
        tosserAppDelegate *del = (tosserAppDelegate *)delegate;
        [del showFeedback];
    }
    else if(idx == INFO_WEBSITE) {
        [app openURL:[NSURL URLWithString:@"http://farmageddongame.com"]];
    }
    else if(idx == INFO_TWITTER) {
        [app openURL:[NSURL URLWithString:@"http://twitter.com/farmageddongame"]];
    }
    else if(idx == INFO_DEVELOPER) {
        [app openURL:[NSURL URLWithString:@"http://jlongster.com"]];
    }

    [infoAlert release];
    infoAlert = nil;
}

- (void)gotoFullVersion {
    UIApplication *app = [UIApplication sharedApplication];
    [app openURL:[NSURL URLWithString:@"http://itunes.apple.com/us/app/farmageddon/id365634742?mt=8"]];
}

- (void)hideHighScoreField {
	highScoreName.hidden = YES;
}

- (void)showHighScoreField:(int)x y:(int)y {
	highScoreName.hidden = NO;
	
	CGRect r;
	r.origin = CGPointMake(x,y);
	r.size = highScoreName.frame.size;
	highScoreName.frame = r;
}

- (char*)highScoreFieldValue {
	char* name = (char*)malloc(1024);
	[highScoreName.text getCString:name maxLength:1024 encoding:NSASCIIStringEncoding];
	return name;
}

- (void)submitHighScore:(int)score {
	[OFHighScoreService setHighScore: score
						forLeaderboard:@"241623"
						onSuccess:OFDelegate()
						onFailure:OFDelegate()];
}

- (void)fetchGlobalScores {
	[feintDelegate loadHighScores];
}

//// Touches

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    touches_began(touches, event);
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
    touches_moved(touches, event);
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
    touches_ended(touches, event);
}

- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event {
    touches_cancelled(touches, event);
}

//// Rendering

- (void)feintOpenDashboard {
    [OpenFeint launchDashboardWithHighscorePage:@"241623"];
}

- (void)drawView {
    [EAGLContext setCurrentContext:context];

    glBindFramebufferOES(GL_FRAMEBUFFER_OES, viewFramebuffer);
    glViewport(0, 0, backingWidth, backingHeight);

    render();

    glBindRenderbufferOES(GL_RENDERBUFFER_OES, viewRenderbuffer);
    [context presentRenderbuffer:GL_RENDERBUFFER_OES];
}

- (void)layoutSubviews {
	[EAGLContext setCurrentContext:context];
	[self destroyFramebuffer];
	[self createFramebuffer];
	[self drawView];
}

- (BOOL)createFramebuffer {
	glGenFramebuffersOES(1, &viewFramebuffer);
	glGenRenderbuffersOES(1, &viewRenderbuffer);
	
	glBindFramebufferOES(GL_FRAMEBUFFER_OES, viewFramebuffer);
	glBindRenderbufferOES(GL_RENDERBUFFER_OES, viewRenderbuffer);
	[context renderbufferStorage:GL_RENDERBUFFER_OES fromDrawable:(CAEAGLLayer*)self.layer];
	glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_COLOR_ATTACHMENT0_OES, GL_RENDERBUFFER_OES, viewRenderbuffer);
	
	glGetRenderbufferParameterivOES(GL_RENDERBUFFER_OES, GL_RENDERBUFFER_WIDTH_OES, &backingWidth);
	glGetRenderbufferParameterivOES(GL_RENDERBUFFER_OES, GL_RENDERBUFFER_HEIGHT_OES, &backingHeight);
	
	if(USE_DEPTH_BUFFER) {
		glGenRenderbuffersOES(1, &depthRenderbuffer);
		glBindRenderbufferOES(GL_RENDERBUFFER_OES, depthRenderbuffer);
		glRenderbufferStorageOES(GL_RENDERBUFFER_OES, GL_DEPTH_COMPONENT16_OES, backingWidth, backingHeight);
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_DEPTH_ATTACHMENT_OES, GL_RENDERBUFFER_OES, depthRenderbuffer);
	}
    
	glBindRenderbufferOES(GL_RENDERBUFFER_OES, viewRenderbuffer);
    
	if(glCheckFramebufferStatusOES(GL_FRAMEBUFFER_OES) != GL_FRAMEBUFFER_COMPLETE_OES) {
		NSLog(@"failed to make complete framebuffer object %x", glCheckFramebufferStatusOES(GL_FRAMEBUFFER_OES));
		return NO;
	}
	
	return YES;
}


- (void)destroyFramebuffer {
	
	glDeleteFramebuffersOES(1, &viewFramebuffer);
	viewFramebuffer = 0;
	glDeleteRenderbuffersOES(1, &viewRenderbuffer);
	viewRenderbuffer = 0;
	
	if(depthRenderbuffer) {
		glDeleteRenderbuffersOES(1, &depthRenderbuffer);
		depthRenderbuffer = 0;
	}
}


- (void)startAnimation {
	self.animationTimer = [NSTimer scheduledTimerWithTimeInterval:animationInterval target:self selector:@selector(drawView) userInfo:nil repeats:YES];
}


- (void)stopAnimation {
	self.animationTimer = nil;
}


- (void)setAnimationTimer:(NSTimer *)newTimer {
	[animationTimer invalidate];
	animationTimer = newTimer;
}


- (void)setAnimationInterval:(NSTimeInterval)interval {
	
	animationInterval = interval;
	if (animationTimer) {
		[self stopAnimation];
		[self startAnimation];
	}
}


- (void)dealloc {
	
	[self stopAnimation];
	
	if ([EAGLContext currentContext] == context) {
		[EAGLContext setCurrentContext:nil];
	}
	
	[context release];	
	[super dealloc];
}

@end
