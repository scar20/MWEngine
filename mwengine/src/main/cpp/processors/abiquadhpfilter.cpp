//
// Created by Sylvain on 2021-04-20.
//

#include "abiquadhpfilter.h"
#include "../global.h"
#include <math.h>

namespace MWEngine {

/**
 * @param aCutoffFrequency {float} desired cutoff frequency in Hz range 20-20000hz
 * @param aResonance {float} resonance range -25/25db
 * @param aMinFreq {float} minimum cutoff frequency in Hz
 * @param aMaxFreq {float} maximum cutoff frequency in Hz
 * @param numChannels {int} amount of output channels
 */
    ABiquadHPFilter::ABiquadHPFilter( float aCutoffFrequency, float aResonance,
                                      float aMinFreq, float aMaxFreq, int numChannels )
    {
        _resonance       = aResonance;
        _minFreq         = aMinFreq;
        _maxFreq         = aMaxFreq;
        amountOfChannels = numChannels;

        init( aCutoffFrequency );
    }

    ABiquadHPFilter::ABiquadHPFilter()
    {
        // resonance range -25/25db
        _resonance       = 0; //( float ) sqrt( 1 ) / 2;
        _minFreq         = 20.f;
        _maxFreq         = 20000; //AudioEngineProps::SAMPLE_RATE / 8;
        amountOfChannels = AudioEngineProps::OUTPUT_CHANNELS;

        init( _maxFreq );
    }

    ABiquadHPFilter::~ABiquadHPFilter()
    {
        delete[] in1;
        delete[] in2;
        delete[] out1;
        delete[] out2;

        in1 = in2 = out1 = out2 = nullptr;
    }

/* public methods */

    void ABiquadHPFilter::process( AudioBuffer* sampleBuffer, bool isMonoSource )
    {
        int bufferSize               = sampleBuffer->bufferSize;


        if ( amountOfChannels < sampleBuffer->amountOfChannels )
            isMonoSource = true;

        for ( int i = 0, l = sampleBuffer->amountOfChannels; i < l; ++i )
        {
            SAMPLE_TYPE* channelBuffer = sampleBuffer->getBufferForChannel( i );

            calculateParameters();

            for ( int j = 0; j < bufferSize; ++j )
            {
                SAMPLE_TYPE input = channelBuffer[ j ];
                output            = a0 * input + a1 * in1[ i ] + a2 * in2[ i ] - b1 * out1[ i ] - b2 * out2[ i ];

                in2 [ i ] = in1[ i ];
                in1 [ i ] = input;
                out2[ i ] = out1[ i ];
                out1[ i ] = output;

                // commit the effect
                channelBuffer[ j ] = output;
            }

            // save CPU cycles when source is mono
            if ( isMonoSource )
            {
                sampleBuffer->applyMonoSource();
                break;
            }
        }
    }

    void ABiquadHPFilter::setCutoff( float frequency )
    {
        _cutoff     = std::max( _minFreq, std::min( frequency, _maxFreq ));
        calculateParameters();
    }

    float ABiquadHPFilter::getCutoff()
    {
        return _cutoff;
    }

    void ABiquadHPFilter::setResonance( float resonance )
    {
        _resonance = resonance;
        calculateParameters();
    }

    float ABiquadHPFilter::getResonance()
    {
        return _resonance;
    }

/* private methods */

    void ABiquadHPFilter::init( float cutoff )
    {
        SAMPLE_RATE = ( float ) AudioEngineProps::SAMPLE_RATE;

        _cutoff     = _maxFreq;

        in1  = new SAMPLE_TYPE[ amountOfChannels ];
        in2  = new SAMPLE_TYPE[ amountOfChannels ];
        out1 = new SAMPLE_TYPE[ amountOfChannels ];
        out2 = new SAMPLE_TYPE[ amountOfChannels ];

        for ( int i = 0; i < amountOfChannels; ++i )
        {
            in1 [ i ] = 0.0;
            in2 [ i ] = 0.0;
            out1[ i ] = 0.0;
            out2[ i ] = 0.0;
        }

        // using this setter caches appropriate values
        setCutoff( cutoff );
    }

    void ABiquadHPFilter::calculateParameters()
    {
        double cutoff, res;
        cutoff = 2 * _cutoff / SAMPLE_RATE;
        res = pow(10, 0.05 * -_resonance);
        k = 0.5 * res * sin(PI * cutoff);
        c1 = 0.5 * (1 - k) / (1 + k);
        c2 = (0.5 + c1) * cos(PI * cutoff);
        c3 = (0.5 + c1 + c2) * 0.25; // hi-pass


        a0 = 2 * c3;
        a1 = -4 * c3; // hi-pass
        a2 = 2 * c3;
        b1 = -2 * c2; // hi-pass
        b2 = 2 * c1;
    }

} // E.O namespace MWEngine
