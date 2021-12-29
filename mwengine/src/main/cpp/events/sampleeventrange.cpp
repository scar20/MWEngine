//
// Created by Sylvain on 2021-12-28.
//

#include "sampleeventrange.h"
#include <utilities/bufferutility.h>
#include "../audioengine.h"
#include "../global.h"
#include "../sequencer.h"
#include <messaging/notifier.h>
#include <definitions/notifications.h>

namespace MWEngine {

/* constructor / destructor */

    SampleEventRange::SampleEventRange()
    {
        init( nullptr );
    }

    SampleEventRange::SampleEventRange(BaseInstrument *aInstrument)
    {
        init( aInstrument );
    }

    SampleEventRange::~SampleEventRange() {
        // nowt...
    }

/* public methods */

    void SampleEventRange::play() {
        // when invoking play() ensure the read pointers
        // are back at the start (when looping, at the beginning of
        // the entire sample, when playing from a range, at the beginning of the range)
        if (_isForward) {
            _readPointer = std::max(0, _bufferRangeStart);
            _readPointerF = (float) _readPointer;
            _rangePointerF = _readPointerF; // seem this is the fix for range not reset to start on play
            _lastPlaybackPosition = _bufferRangeStart;

        } else { // backward
            _readPointer = std::min(_buffer->bufferSize - 1, _bufferRangeEnd);
            _readPointerF = (float) _readPointer;
            _rangePointerF = _readPointerF; // seem this is the fix for range not reset to start on play
            _lastPlaybackPosition = getBufferRangeEnd(); //_bufferRangeEnd;
        }

        BaseAudioEvent::play();
    }

// event length in sample
    void SampleEventRange::setEventLength(int value) {
        if (_loopeable) {

            _eventLength = value;

            // loopeable-events differ from non-loopable events in that
            // we allow the events end to exceed the range of its start
            // plus the total sample duration (the delta will be filled with
            // looped content)

            if (_eventEnd <= _eventStart ||
                _eventEnd < (_eventStart + _eventLength)) {
                _eventEnd = _eventStart + (_eventLength - 1);
            }

            // update end position in seconds
            _endPosition = BufferUtility::bufferToSeconds(_eventEnd, AudioEngineProps::SAMPLE_RATE);
        } else {
            BaseAudioEvent::setEventLength(value);
        }
    }

// event end in sample
    void SampleEventRange::setEventEnd(int value) {
        if (!_loopeable) {
            BaseAudioEvent::setEventEnd(value);
            return;
        }

        _eventEnd = value;

        // update end position in seconds
        _endPosition = BufferUtility::bufferToSeconds(_eventEnd, AudioEngineProps::SAMPLE_RATE);
    }

    int SampleEventRange::getBufferRangeStart() {
        return _bufferRangeStart;
    }

// buffer range start in sample
    void SampleEventRange::setBufferRangeStart(int value) {
        _bufferRangeStart = (_bufferRangeEnd > 0) ? std::min(value, _bufferRangeEnd - 1) : value;

        // integer used for non altered playback rate

        if (_rangePointer < _bufferRangeStart)
            _rangePointer = _bufferRangeStart;

        // floating point used for alternate playback rates

        if (_rangePointerF < _bufferRangeStart)
            _rangePointerF = (float) _bufferRangeStart;

        if (_bufferRangeEnd <= _bufferRangeStart)
            _bufferRangeEnd = std::max(_bufferRangeStart + (_bufferRangeLength - 1),
                                       _bufferRangeStart);

        // buffer range may never exceed the length of the source buffer (which can be unequal to the sample length)

        if (_buffer != nullptr && _bufferRangeEnd >= _buffer->bufferSize)
            setBufferRangeEnd(_buffer->bufferSize - 1);

        _bufferRangeLength = (_bufferRangeEnd - _bufferRangeStart) + 1;

    }

    int SampleEventRange::getBufferRangeEnd() {
        return (_playbackRate == 1.f) ? _bufferRangeEnd : _bufferRangeStart +
                                                          getBufferRangeLength();
    }

// buffer range end in sample
    void SampleEventRange::setBufferRangeEnd(int value) {
        // buffer range may never exceed the length of the source buffer (which can be unequal to the sample length)
        _bufferRangeEnd = (_buffer != nullptr) ? std::min(value, _buffer->bufferSize - 1) : value;

        if (_rangePointer > _bufferRangeEnd)
            _rangePointer = _bufferRangeEnd;

        if (_rangePointerF > getBufferRangeEnd())
            _rangePointerF = (float) getBufferRangeEnd();

        if (_bufferRangeStart >= _bufferRangeEnd)
            _bufferRangeStart = std::max(_bufferRangeEnd - 1, 0);

        _bufferRangeLength = (_bufferRangeEnd - _bufferRangeStart) + 1;

    }

    int SampleEventRange::getBufferRangeLength() {
        return (_playbackRate == 1.f) ? _bufferRangeLength : (int) ((float) _bufferRangeLength /
                                                                    _playbackRate);
    }

/**
 * return the current playback position for a live event
 * e.g. a SampleEventRange triggered by play()
 */
    int SampleEventRange::getPlaybackPosition() {
        return _lastPlaybackPosition;
    }

    unsigned int SampleEventRange::getSampleRate() {
        return _sampleRate;
    }

    bool SampleEventRange::setSample(AudioBuffer *sampleBuffer) {
        return setSample(sampleBuffer, AudioEngineProps::SAMPLE_RATE);
    }

// set eventLenght, eventEnd, bufferRangeStart, bufferRangeEnd, loopStartoffset, loopEndOffset
    bool SampleEventRange::setSample(AudioBuffer *sampleBuffer, unsigned int sampleRate) {
        if (sampleBuffer == nullptr)
            return false;

        // make sure we lock read/write operations as setting a sample
        // while the engine is running (thus reading from the current one) is a tad dangerous ;)

        bool wasLocked = _locked;
        _locked = true;

        int sampleLength = sampleBuffer->bufferSize;

        // delete previous contents
        if (_eventLength != sampleLength)
            destroyBuffer();

        // is this events buffer destroyable ? then clone
        // the input buffer, if not, merely point to it to
        // minimize memory consumption when re-using existing samples

        if (_destroyableBuffer)
            _buffer = sampleBuffer->clone();
        else
            _buffer = sampleBuffer;

        _buffer->loopeable = _loopeable;
        setEventLength(sampleLength);
        setEventEnd(_eventStart + (_eventLength - 1));

        // in case the given event has a sample rate that differs from the engine
        // adjust the playback rate of the sample accordingly

//        ToDo: add quality sample rate conversion algo at sample load so everything is at engine rate
        _sampleRate = sampleRate;
        if (_sampleRate != AudioEngineProps::SAMPLE_RATE) {
            setPlaybackRate(
                    _playbackRate / (float) AudioEngineProps::SAMPLE_RATE * (float) _sampleRate);
        }

        // when switching samples, existing buffer ranges are reset

        _bufferRangeStart = 0;
        setBufferRangeEnd(_bufferRangeStart + (_eventLength - 1)); // also updates range length

        _updateAfterUnlock = false; // unnecessary

        if (!wasLocked)
            _locked = false;

        return true;
    }

// EXPERIMENTAL playback direction
    void SampleEventRange::setPlaybackDirection(bool forward) {
        _isForward = forward;
    }

    void SampleEventRange::setID(int id) {
        _id = id;
    }

    float SampleEventRange::getPlaybackRate() {
        return _playbackRate;
    }

// playbackRate is a factor applied on sampleRate; 0.5 = half, 1.0 = equal, 2.0 = double
    void SampleEventRange::setPlaybackRate(float value) {
        // allow only 100x slowdown and speed up
        _playbackRate = std::max(0.01f, std::min(100.f, value));
    }

    bool SampleEventRange::isLoopeable() {
        return _loopeable;
    }

    void SampleEventRange::setLoopeable(bool value, int crossfadeInMilliseconds) {
        _loopeable = value;

        if (_buffer != nullptr)
            _buffer->loopeable = _loopeable;

        _crossfadeMs = crossfadeInMilliseconds;
        cacheFades();
    }

// read pointer position in sample
    int SampleEventRange::getReadPointer() {
        return _readPointer;
    }


// event length in sample; can be factored by playbackRate
    int SampleEventRange::getEventLength() {
        return (_playbackRate == 1.f || _loopeable) ? _eventLength : (int) ((float) _eventLength /
                                                                            _playbackRate);
    }

// event length in sample
    int SampleEventRange::getOriginalEventLength() {
        return _eventLength;
    }

// event end position in sample; can be factored by eventLength
    int SampleEventRange::getEventEnd() {
        return (_playbackRate == 1.f || _loopeable) ? _eventEnd : _eventStart + getEventLength();
    }

    /**
 * Invoked by the Sequencer in case this event isn't sequenced
 * but triggered manually via a "noteOn" / "noteOff" operation for instant "live" playback
 */
    void SampleEventRange::mixBuffer(AudioBuffer *outputBuffer) {

        // write sample contents into live buffer
        // we specify the maximum buffer position as the full sample playback range
        mixBuffer(outputBuffer, _lastPlaybackPosition, 0, _eventLength);

        // Forgot to add test for forward/backward here so here it is
        if (_isForward)
        {
            if ((_lastPlaybackPosition += outputBuffer->bufferSize) >= _bufferRangeStart + getBufferRangeLength()) {
                // if this is a one-shot SampleEventRange, remove it from the sequencer when we have exceeded
                // the sample length (e.g. played it in its entirety)

                if (!_loopeable) {
                    stop();
                    Notifier::broadcast(Notifications::MARKER_POSITION_REACHED, _id );
                    __android_log_print(ANDROID_LOG_DEBUG, TAG_SAMPLE,
                                        "SampleEventRange::getBufferForRange END_REACHED, forward");
                }
                else
                    _lastPlaybackPosition = std::max(_bufferRangeStart,
                                                     _lastPlaybackPosition - getBufferRangeLength());
            }
        }
        else // backward
        {
            if ((_lastPlaybackPosition -= outputBuffer->bufferSize) <= getBufferRangeStart()) {
                // if this is a one-shot SampleEventRange, remove it from the sequencer when we have exceeded
                // the sample length (e.g. played it in its entirety)

                if (!_loopeable) {
                    stop();
                    Notifier::broadcast(Notifications::MARKER_POSITION_REACHED, _id);
                    __android_log_print(ANDROID_LOG_DEBUG, TAG_SAMPLE,
                                        "SampleEventRange::getBufferForRange END_REACHED, backward");
                }
                else
                    _lastPlaybackPosition = std::max(_bufferRangeEnd,
                                                     _lastPlaybackPosition + getBufferRangeLength());
            }
        }
    }

    void SampleEventRange::mixBuffer(AudioBuffer *outputBuffer, int playbackPos,
                                int minBufferPosition, int maxBufferPosition) {

//        __android_log_print(ANDROID_LOG_DEBUG, TAG_SAMPLE,
//                            "SampleEventRange::mixBuffer arg playbackPosition: %d "
//                            "chnlminBufferPosition: %d chnlmaxBufferPosition: %d",
//                            playbackPosition, minBufferPosition, maxBufferPosition);
        if (!hasBuffer())
            return;

        int bufferSize = outputBuffer->bufferSize;
        int amountOfChannels = outputBuffer->amountOfChannels;
        bool gotBuffer = false;
        bool monoCopy = _buffer->amountOfChannels < amountOfChannels;

        bool useInternalPointer = _loopeable;

//        if (useInternalPointer) // alway use internalPointer
        playbackPos = _readPointer;

//        int eventStart = _eventStart;
//        int eventEnd = getEventEnd();
        // replace assignment to point to bufferRange instead of eventStart/End
        int eventStart = getBufferRangeStart();
//        int eventEnd = getBufferRangeEnd();
        int eventEnd = _bufferRangeEnd;

        SAMPLE_TYPE *srcBuffer;

        int t;
        SAMPLE_TYPE s1, s2;
        float bufferRangeEnd = (float) getBufferRangeEnd();
        float bufferRangeStart = (float) getBufferRangeStart();
        float frac;

        if (_isForward) { // EXPERIMENTAL playback direction
            for (int i = 0; i < bufferSize; ++i) {
                // read sample when the read pointer is within sample start and end points
                if (playbackPos >= eventStart && playbackPos <= eventEnd) {
                    t = (int) _rangePointerF;
                    frac = _rangePointerF - t; // between 0 - 1 range

                    // use range pointers to read within the specific sample ranges
                    for (int c = 0; c < amountOfChannels; ++c) {
                        // this sample might have less channels than the output outputBuffer
                        if (!monoCopy)
                            srcBuffer = _buffer->getBufferForChannel(c);
                        else
                            srcBuffer = _buffer->getBufferForChannel(0);

                        s1 = srcBuffer[t];
                        s2 = srcBuffer[t + 1];

                        SAMPLE_TYPE *targetBuffer = outputBuffer->getBufferForChannel(c);
                        targetBuffer[i] += ((s1 + (s2 - s1) * frac) * _volume);
                    }

                    if ((_rangePointerF += _playbackRate) > (float) _bufferRangeEnd)
                        _rangePointerF = (float) _bufferRangeStart;

                    gotBuffer = true;
                }

                // if this is a loopeable sample (thus using internal read pointer)
                // set the read pointer to the sample start so it keeps playing indefinitely

//                if (++playbackPos > eventEnd && _loopeable)
//                    playbackPos = eventStart;
                if (++playbackPos > getEventEnd() && _loopeable)
                    playbackPos = eventStart;

            }
        } else { // backward

            for (int i = 0; i < bufferSize; ++i) {
                // read sample when the read pointer is within sample start and end points
                if (playbackPos >= eventStart && playbackPos <= eventEnd) {
                    t = (int) _rangePointerF;
                    frac = _rangePointerF - t; // between 0 - 1 range

                    // use range pointers to read within the specific sample ranges
                    for (int c = 0; c < amountOfChannels; ++c) {
                        // this sample might have less channels than the output outputBuffer
                        if (!monoCopy)
                            srcBuffer = _buffer->getBufferForChannel(c);
                        else
                            srcBuffer = _buffer->getBufferForChannel(0);

                        s1 = srcBuffer[t];
                        s2 = srcBuffer[t + 1];

                        SAMPLE_TYPE *targetBuffer = outputBuffer->getBufferForChannel(c);
                        targetBuffer[i] += ((s1 + (s2 - s1) * frac) * _volume);
                    }

                    if ((_rangePointerF -= _playbackRate) < bufferRangeStart)
                        _rangePointerF = (float) _bufferRangeEnd;

                    gotBuffer = true;
                }

                // if this is a loopeable sample (thus using internal read pointer)
                // set the read pointer to the sample end so it keeps playing indefinitely

                if (--playbackPos < eventStart && _loopeable)
                    playbackPos = eventEnd;
//                if (--playbackPos < eventStart && _loopeable)
//                    playbackPos = getBufferRangeEnd();

            }
        }

//        __android_log_print(ANDROID_LOG_DEBUG, TAG_SAMPLE, "SampleEventRange::getBufferForRange playbackPos %d", playbackPos);

        // hack - always update _readPointer
//        if (useInternalPointer)
//        _lastPlaybackPosition = playbackPos;
        _readPointer = (int)_rangePointerF; // report -RangePointerF instead;

        return;
    }

/* protected methods */

    void SampleEventRange::init( BaseInstrument* instrument )
    {
        BaseAudioEvent::init();

        _isForward            = true;
        _id                   = 0;

        _bufferRangeStart     = 0;
        _bufferRangeEnd       = 0;
        _bufferRangeLength    = 0;
        _loopeable            = false;
        _crossfadeMs          = 0;
        _crossfadeStart       = 0;
        _crossfadeEnd         = 0;
        _readPointer          = 0;

        _rangePointer         = 0;     // integer for non altered playback rates
        _rangePointerF        = 0.f;   // floating point for alternate playback rates
        _lastPlaybackPosition = 0;
        _playbackRate         = 1.f;
        _readPointerF         = 0.f;
        _destroyableBuffer    = false; // is referenced via SampleManager !
        _instrument           = instrument;
        _sampleRate           = ( unsigned int ) AudioEngineProps::SAMPLE_RATE;
    }

    void SampleEventRange::cacheFades() {
        if (_crossfadeMs > 0) {

            // calculate the amount of samples we deem satisfactory to prevent popping at non-zero crossings
            int samplesToFade = BufferUtility::millisecondsToBuffer(_crossfadeMs,
                                                                    AudioEngineProps::SAMPLE_RATE);
            _crossfadeStart = _bufferRangeEnd - samplesToFade; // at end of sample, prior to looping
            _crossfadeEnd = _bufferRangeStart + samplesToFade; // from beginning of loop start offset
        } else {
            _crossfadeStart = _bufferRangeEnd;
            _crossfadeEnd = 0;
        }
    }

}