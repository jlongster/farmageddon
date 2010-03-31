;;;; OpenAL interface
;;; OK, don't harass me here. This is awful, and I know it. Sound is a
;;; whole task which I wasn't prepared for. This barely gets me up and
;;; running.

(include "al#.scm")

(c-declare #<<end-c-code
 
 #import <OpenAL/al.h>
 #import <OpenAL/alc.h>
 #import <AudioToolbox/AudioToolbox.h>
 #import <AudioToolbox/ExtendedAudioFile.h>
 
 struct AudioData {
    void* data;
    int size;
    int format;
    int sampleRate;
 };

 typedef struct AudioData AudioData;
 
 typedef ALvoid AL_APIENTRY (*alBufferDataStaticProcPtr) (const ALint bid, ALenum format, ALvoid* data, ALsizei size, ALsizei freq);
 AudioData load_audio_data(char *inFile);

 ALvoid alBufferDataStaticProc(const ALint bid, ALenum format, ALvoid* data, ALsizei size, ALsizei freq) {
     static alBufferDataStaticProcPtr proc = NULL;
 
     if (proc == NULL) {
         proc = (alBufferDataStaticProcPtr) alcGetProcAddress(NULL, (const ALCchar*) "alBufferDataStatic");
     }
 
     if (proc) proc(bid, format, data, size, freq);
 }

 ALCdevice *device;
 ALCcontext *context;
 
 void init_audio() {
     device = alcOpenDevice(NULL);
     if(device) {
         context = alcCreateContext(device, NULL);
         alcMakeContextCurrent(context);
     }
 }

 void shutdown_audio() {
     alcDestroyContext(context);
     alcCloseDevice(device);
 }

 GLuint load_audio(char *file) {
     AudioData data_desc;
     data_desc = load_audio_data(file);
     
     if(!data_desc.data) {
         return 0;
     }

     ALuint bufferId;
     alGenBuffers(1, &bufferId);
     alBufferData(bufferId,
                  data_desc.format,
                  data_desc.data,
                  data_desc.size,
                  data_desc.sampleRate);
     free(data_desc.data);
     return bufferId;
 }

 GLuint make_audio_source(GLuint bufferId) {
     GLuint sourceId;
     alGenSources(1, &sourceId);
     alSourcei(sourceId, AL_BUFFER, bufferId);
     alSourcef(sourceId, AL_PITCH, 1.0f);
     alSourcef(sourceId, AL_GAIN, 1.0f);

     return sourceId;
 }

 void free_audio_source(GLuint sourceId) {
     alDeleteSources(1, &sourceId);
 }

 void free_audio_buffer(GLuint bufferId) {
     alDeleteBuffers(1, &bufferId);
 }
 
 void play_audio(GLuint sourceId) {
     alSourcePlay(sourceId);
 }

 void stop_audio(GLuint sourceId) {
     alSourceStop(sourceId);
 }

 void rewind_audio(GLuint sourceId) {
     alSourceRewind(sourceId);
 }

 int is_audio_playing(GLuint sourceId) {
     ALenum state;
     alGetSourcei(sourceId, AL_SOURCE_STATE, &state);
     return state == AL_PLAYING;
 }
 
 AudioData load_audio_data(char *inFile) {
     CFStringRef name = CFStringCreateWithCString(NULL, inFile, kCFStringEncodingUTF8);
     CFURLRef inFileURL = CFBundleCopyResourceURL(CFBundleGetMainBundle(), name, NULL, NULL);
     CFRelease(name);
    
     OSStatus err = noErr;
     SInt64 theFileLengthInFrames = 0;
     AudioStreamBasicDescription theFileFormat;
     UInt32 thePropertySize = sizeof(theFileFormat);
     ExtAudioFileRef extRef = NULL;
     void* theData = NULL;
     AudioStreamBasicDescription theOutputFormat;
     AudioData result;
    
     // Open a file with ExtAudioFileOpen()
     err = ExtAudioFileOpenURL(inFileURL, &extRef);
     if(err) { printf("MyGetOpenALAudioData: ExtAudioFileOpenURL FAILED, Error = %ld\n", (long int)err); goto Exit; }
     CFRelease(inFileURL);
 
     // Get the audio data format
     err = ExtAudioFileGetProperty(extRef, kExtAudioFileProperty_FileDataFormat, &thePropertySize, &theFileFormat);
     if(err) { printf("MyGetOpenALAudioData: ExtAudioFileGetProperty(kExtAudioFileProperty_FileDataFormat) FAILED, Error = %ld\n", (long int)err); goto Exit; }
     if (theFileFormat.mChannelsPerFrame > 2)  { printf("MyGetOpenALAudioData - Unsupported Format, channel count is greater than stereo\n"); goto Exit;}
 
     // Set the client format to 16 bit signed integer (native-endian) data
     // Maintain the channel count and sample rate of the original source format
     theOutputFormat.mSampleRate = theFileFormat.mSampleRate;
     theOutputFormat.mChannelsPerFrame = theFileFormat.mChannelsPerFrame;
 
     theOutputFormat.mFormatID = kAudioFormatLinearPCM;
     theOutputFormat.mBytesPerPacket = 2 * theOutputFormat.mChannelsPerFrame;
     theOutputFormat.mFramesPerPacket = 1;
     theOutputFormat.mBytesPerFrame = 2 * theOutputFormat.mChannelsPerFrame;
     theOutputFormat.mBitsPerChannel = 16;
     theOutputFormat.mFormatFlags = kAudioFormatFlagsNativeEndian | kAudioFormatFlagIsPacked | kAudioFormatFlagIsSignedInteger;
 
     // Set the desired client (output) data format
     err = ExtAudioFileSetProperty(extRef, kExtAudioFileProperty_ClientDataFormat, sizeof(theOutputFormat), &theOutputFormat);
     if(err) { printf("MyGetOpenALAudioData: ExtAudioFileSetProperty(kExtAudioFileProperty_ClientDataFormat) FAILED, Error = %ld\n", (long int)err); goto Exit; }
 
     // Get the total frame count
     thePropertySize = sizeof(theFileLengthInFrames);
     err = ExtAudioFileGetProperty(extRef, kExtAudioFileProperty_FileLengthFrames, &thePropertySize, &theFileLengthInFrames);
     if(err) { printf("MyGetOpenALAudioData: ExtAudioFileGetProperty(kExtAudioFileProperty_FileLengthFrames) FAILED, Error = %ld\n", (long int)err); goto Exit; }

       // Read all the data into memory
     UInt32 dataSize = theFileLengthInFrames * theOutputFormat.mBytesPerFrame;
     theData = malloc(dataSize);
     if (theData)
         {
             AudioBufferList		theDataBuffer;
             theDataBuffer.mNumberBuffers = 1;
             theDataBuffer.mBuffers[0].mDataByteSize = dataSize;
             theDataBuffer.mBuffers[0].mNumberChannels = theOutputFormat.mChannelsPerFrame;
             theDataBuffer.mBuffers[0].mData = theData;
 
             // Read the data into an AudioBufferList
             err = ExtAudioFileRead(extRef, (UInt32*)&theFileLengthInFrames, &theDataBuffer);
             if(err == noErr) {
                 // success
                 result.size = (ALsizei)dataSize;
                 result.format = (theOutputFormat.mChannelsPerFrame > 1) ? AL_FORMAT_STEREO16 : AL_FORMAT_MONO16;
                 result.sampleRate = (ALsizei)theOutputFormat.mSampleRate;
             }
             else {
                 // failure
                 free(theData);
                 theData = NULL; // make sure to return NULL
                 printf("MyGetOpenALAudioData: ExtAudioFileRead FAILED, Error = %ld\n", (long int)err); goto Exit;
             }
         }
 
 Exit:
     // Dispose the ExtAudioFileRef, it is no longer needed
     if (extRef) ExtAudioFileDispose(extRef);
     result.data = theData;
     return result;
 }

end-c-code
)

(define AL_PITCH 4099)
(define AL_GAIN 4106)
     
(define init-audio
  (c-lambda () void "init_audio"))
    
(define shutdown-audio
  (c-lambda () void "shutdown_audio"))

(define load-audio
  (c-lambda (char-string) GLuint "load_audio"))

(define make-audio-source
  (c-lambda (unsigned-int) GLuint "make_audio_source"))

(define free-audio-buffer
  (c-lambda (unsigned-int) void "free_audio_buffer"))

(define free-audio-source
  (c-lambda (unsigned-int) void "free_audio_source"))

(define %play-audio
  (c-lambda (unsigned-int) void "play_audio"))

(define (play-audio source)
  (if *play-audio* (%play-audio source)))

(define (play-and-release-audio source)
  (if *play-audio*
      (begin
        (play-audio source)
        (thread-start!
         (make-thread
          (lambda ()
            (let loop ()
              (if (is-audio-playing? source)
                  (begin
                    (thread-sleep! .2)
                    (loop))
                  (begin
                    (stop-audio source)
                    (free-audio-source source))))))))
      (free-audio-source source)))
      
(define stop-audio
  (c-lambda (unsigned-int) void "stop_audio"))

(define *play-audio* #t)

(define (mute-audio)
  (set! *play-audio* #f))

(define (unmute-audio)
  (set! *play-audio* #t))

(define (is-audio-muted?)
  (not *play-audio*))

(define rewind-audio
  (c-lambda (unsigned-int) void "rewind_audio"))

(define is-audio-playing?
  (c-lambda (unsigned-int) bool "is_audio_playing"))

(define alSourcei
  (c-lambda (unsigned-int int int) void  "alSourcei"))

(define alSourcef
  (c-lambda (unsigned-int int float) void  "alSourcef"))

;;; AudioData structure

;; (define AudioData-data
;;   (c-lambda (AudioData) void-array "___result_voidstar = ___arg1.data;"))

;; (define AudioData-size
;;   (c-lambda (AudioData) int "___result = ___arg1.size;"))

;; (define AudioData-format
;;   (c-lambda (AudioData) int "___result = ___arg1.format;"))

;; (define AudioData-sample-rate
;;   (c-lambda (AudioData) int "___result = ___arg1.sampleRate;"))
