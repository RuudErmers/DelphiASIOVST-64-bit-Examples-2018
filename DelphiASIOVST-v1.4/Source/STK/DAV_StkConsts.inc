const
(*
  NOPE    =-32767;
  YEP     = 1;
  SK_DBL  =-32766;
  SK_INT  =-32765;
  SK_STR  =-32764;
*)
  CMidiNoteOff                = 128;
  CMidiNoteOn                 = 144;
  CMidiPolyPressure           = 160;
  CMidiControlChange          = 176;
  CMidiProgramChange          = 192;
  CMidiAfterTouch             = 208;
  CMidiChannelPressure        = CMidiAfterTouch;
  CMidiPitchWheel             = 224;
  CMidiPitchBend              = CMidiPitchWheel;
  CMidiPitchChange            = 249;
  CMidiClock                  = 248;
  CMidiSongStart              = 250;
  CMidiContinue               = 251;
  CMidiSongStop               = 252;
  CMidiActiveSensing          = 254;
  CMidiSystemReset            = 255;
  CMidiVolume                 = 7;
  CMidiModWheel               = 1;
  CMidiModulation             = CMidiModWheel;
  CMidiBreath                 = 2;
  CMidiFootControl            = 4;
  CMidiPortamento             = 65;
  CMidiBalance                = 8;
  CMidiPan                    = 10;
  CMidiSustain                = 64;
  CMidiDamper                 = CMidiSustain;
  CMidiExpression             = 11;
  CMidiAfterTouchContour      = 128;
  CMidiModFrequency           = CMidiExpression;
  CMidiProphesyRibbon         = 16;
  CMidiProphesyWheelUp        = 2;
  CMidiProphesyWheelDown      = 3;
  CMidiProphesyPedal          = 18;
  CMidiProphesyKnob1          = 21;
  CMidiProphesyKnob2          = 22;
  CMidiNoiseLevel             = CMidiFootControl;
  CMidiPickPosition           = CMidiFootControl;
  CMidiStringDamping          = CMidiExpression;
  CMidiStringDetune           = CMidiModWheel;
  CMidiBodySize               = CMidiBreath;
  CMidiBowPressure            = CMidiBreath;
  CMidiBowPosition            = CMidiPickPosition;
  CMidiBowBeta                = CMidiBowPosition;
  CMidiReedStiffness          = CMidiBreath;
  CMidiReedRestPos            = CMidiFootControl;
  CMidiFluteEmbouchure        = CMidiBreath;
  CMidiJetDelay               = CMidiFluteEmbouchure;
  CMidiLipTension             = CMidiBreath;
  CMidiSlideLength            = CMidiFootControl;
  CMidiStrikePosition         = CMidiPickPosition;
  CMidiStickHardness          = CMidiBreath;
  CMidiTrillDepth             = 1051;
  CMidiTrillSpeed             = 1052;
  CMidiStrumSpeed             = CMidiTrillSpeed;
  CMidiRollSpeed              = CMidiTrillSpeed;
  CMidiFilterQ                = CMidiBreath;
  CMidiFilterFreq             = 1062;
  CMidiFilterSweepRate        = CMidiFootControl;
  CMidiShakerInst             = 1071;
  CMidiShakerEnergy           = CMidiBreath;
  CMidiShakerDamping          = CMidiModFrequency;
  CMidiShakerNumObjects       = CMidiFootControl;
  CMidiStrumming              = 1090;
  CMidiNotStrumming           = 1091;
  CMidiTrilling               = 1092;
  CMidiNotTrilling            = 1093;
  CMidiRolling                = CMidiStrumming;
  CMidiNotRolling             = CMidiNotStrumming;
  CMidiPlayerSkill            = 2001;
  CMidiChord                  = 2002;
  CMidiChordOff               = 2003;
  CMidiSingerFilePath         = 3000;
  CMidiSingerFrequency        = 3001;
  CMidiSingerNoteName         = 3002;
  CMidiSingerShape            = 3003;
  CMidiSingerGlot             = 3004;
  CMidiSingerVoicedUnVoiced   = 3005;
  CMidiSingerSynthesize       = 3006;
  CMidiSingerSilence          = 3007;
  CMidiSingerVibratoAmt       = CMidiModWheel;
  CMidiSingerRndVibAmt        = 3008;
  CMidiSingerVibFreq          = CMidiExpression;

const
  Midi2Pitch: array[0..128] of Single = (
       8.18,    8.66,    9.18,    9.72,    10.30,    10.91,    11.56,    12.25,
      12.98,   13.75,   14.57,   15.43,    16.35,    17.32,    18.35,    19.45,
      20.60,   21.83,   23.12,   24.50,    25.96,    27.50,    29.14,    30.87,
      32.70,   34.65,   36.71,   38.89,    41.20,    43.65,    46.25,    49.00,
      51.91,   55.00,   58.27,   61.74,    65.41,    69.30,    73.42,    77.78,
      82.41,   87.31,   92.50,   98.00,   103.83,   110.00,   116.54,   123.47,
     130.81,  138.59,  146.83,  155.56,   164.81,   174.61,   185.00,   196.00,
     207.65,  220.00,  233.08,  246.94,   261.63,   277.18,   293.66,   311.13,
     329.63,  349.23,  369.99,  392.00,   415.30,   440.00,   466.16,   493.88,
     523.25,  554.37,  587.33,  622.25,   659.26,   698.46,   739.99,   783.99,
     830.61,  880.00,  932.33,  987.77,  1046.50,  1108.73,  1174.66,  1244.51,
    1318.51, 1396.91, 1479.98, 1567.98,  1661.22,  1760.00,  1864.66,  1975.53,
    2093.00, 2217.46, 2349.32, 2489.02,  2637.02,  2793.83,  2959.96,  3135.96,
    3322.44, 3520.00, 3729.31, 3951.07,  4186.01,  4434.92,  4698.64,  4978.03,
    5274.04, 5587.65, 5919.91, 6271.93,  6644.88,  7040.00,  7458.62,  7902.13,
    8372.02, 8869.84, 9397.27, 9956.06, 10548.08, 11175.30, 11839.82, 12543.85,
   13289.75);

