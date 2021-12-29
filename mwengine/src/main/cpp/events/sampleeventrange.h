//
// Created by Sylvain on 2021-12-28.
//

#ifndef MWENGINE_SAMPLEEVENTRANGE_H
#define MWENGINE_SAMPLEEVENTRANGE_H


#include "baseaudioevent.h"
#include <instruments/baseinstrument.h>

#include <android/log.h>

#define TAG_SAMPLE "SAMPLE_EVENT"

namespace MWEngine {
    class SampleEventRange : public BaseAudioEvent
    {
    public:
        SampleEventRange();
        SampleEventRange( BaseInstrument* aInstrument );
        virtual ~SampleEventRange();

        void play();

        void setEventLength( int value );
        void setEventEnd( int value );

        virtual int getBufferRangeStart();
        virtual void setBufferRangeStart( int value );
        virtual int getBufferRangeEnd();
        virtual void setBufferRangeEnd( int value );
        virtual int getBufferRangeLength();

        // set the sample that this SampleEvent will playback
        // by default the sample will playback at the sampling rate of the engine

        bool setSample( AudioBuffer* sampleBuffer );

        // EXPERIMENTAL
        // set playback direction
        void setPlaybackDirection(bool forward);

        // convenience
        void setID(int id);

        // use this method in case your samples are at a different sampling rate
        // than the engine (for instance read from WAV file created externally)

        bool setSample( AudioBuffer* sampleBuffer, unsigned int sampleRate );

        float getPlaybackRate();
        void setPlaybackRate( float value );

        // use these to repeat this SampleEvents buffer for the total
        // event duration. Optionally specify the point at which the loop will start
        // for samples where the end and start offsets are not at a zero crossing
        // cross fading can be applied to prevent popping sounds on loop start

        bool isLoopeable();
        void setLoopeable( bool value, int crossfadeInMilliseconds );
        int getReadPointer();


        // custom override allowing the engine to get this events
        // length relative to this playback rate

        int getEventLength();

        int getOriginalEventLength(); // original, untransformed event length
        int getEventEnd();

#ifndef SWIG
        // internal to the engine
        void mixBuffer( AudioBuffer* outputBuffer, int bufferPos, int minBufferPosition, int maxBufferPosition);

        void mixBuffer( AudioBuffer* outputBuffer );
#endif


        int getPlaybackPosition();

        // sample rate of the AudioBuffer that this sample event references
        // this can differ from the AudioEngine's sample rate

        unsigned int getSampleRate();

    protected:

        // play direction
        bool _isForward;
        // convenience
        int _id;

        // total sample range

        int _rangePointer;
        float _rangePointerF;

        // looping / custom repeat range

        bool _loopeable;
        int _crossfadeMs;
        int _crossfadeStart; // the amount of samples to crossfade when about to loop
        int _crossfadeEnd;   // the amount of samples to crossfade when reading from the loop offset
        int _readPointer;    // when loopeable, used internally to keep track of last read buffer offset

        // sample buffer regions (i.e. the sample contents thar are played)

        int _bufferRangeStart;
        int _bufferRangeEnd;
        int _bufferRangeLength;

        float _playbackRate;
        float _lastPlaybackRate; // EXPERIMENT parameter smoothing
        float _readPointerF;

        unsigned int _sampleRate;
        int _lastPlaybackPosition;

        void init( BaseInstrument* aInstrument );
        void cacheFades();
    };
} // E.O namespace MWEngine


#endif //MWENGINE_SAMPLEEVENTRANGE_H
