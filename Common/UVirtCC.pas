unit UVirtCC;

interface

{$ifndef COMPILEMIDIMAPPING }
{$define VSTTransformerVirtCC}
{$include UVstTransformer.inc}
const VirtCC_DELAY_PARAM0 = VirtCC_Effects0;
const VirtCC_DELAY_DEPTH  = VirtCC_Effects0;
    const FullSynthvccs  :array[0..5] of array [0..7] of integer =
       ((VirtCC_OSC1_WAVE,VirtCC_OSC1_FOOT,VirtCC_OSC2_WAVE,VirtCC_OSC2_FOOT,VirtCC_OSC3_WAVE,VirtCC_OSC3_FOOT,VirtCC_OSC2_SEMI,VirtCC_ALL_FAT),
        (VirtCC_LFO1_WAVE,VirtCC_LFO1_RATE,VirtCC_LFO1_DELAY,VirtCC_LFO2_WAVE,VirtCC_LFO2_RATE,VirtCC_LFO2_DELAY, VirtCC_PITCH_MOD,VirtCC_WHEEL_MOD),
        (VirtCC_VCF_CUTOFF,VirtCC_VCF_RESONANCE,VirtCC_VCF_RANGE,VirtCC_VCF_KEYRANGE,VirtCC_OSC1_VOLUME,VirtCC_OSC2_VOLUME,VirtCC_OSC3_VOLUME,VirtCC_Noise_Volume),
        (VirtCC_VCF_ADSR_A,VirtCC_VCF_ADSR_D,VirtCC_VCF_ADSR_S,VirtCC_VCF_ADSR_R,VirtCC_VCA_ADSR_A,VirtCC_VCA_ADSR_D,VirtCC_VCA_ADSR_S,VirtCC_VCA_ADSR_R),
// T_O_D_O: De regel hieronder is FOUT
        (VirtCC_OSC1_WAVE,VirtCC_OSC1_WAVE,VirtCC_OSC1_WAVE,VirtCC_OSC1_WAVE,VirtCC_OSC1_WAVE,VirtCC_OSC1_WAVE, VirtCC_OSC1_WAVE,VirtCC_OSC1_WAVE),
        (VirtCC_DELAY_DEPTH,VirtCC_DELAY_PARAM1,VirtCC_DELAY_PARAM2,VirtCC_REVERB_DEPTH,VirtCC_REVERB_PARAM1,VirtCC_REVERB_PARAM2,VirtCC_GLIDE_RANGE,VirtCC_MAIN_VOLUME));
    const FullSynthlabels  :array[0..5] of array [0..7] of string =
       (('O1 Wave','O1 Foot','O2 Wave','O2 Foot','O3 Wave','O3 Foot','O2 Fine','FAT'),
        ('L1 Wave','L1 Rate','L1 Delay','L2 Wave','L2 Rate','L2 Delay','Breath','Wheel'),
        ('Cutoff','Resonance','EG Range','Key Range','VCO 1','VCO 2','VCO 3','Noise'),
        ('VCF A','VCF D','VCF S','VCF R','VCA A','VCA D','VCA S','VCA R'),
        ('LFO 1','LFO 2','LFO 1','LFO 2','LFO 1','LFO 2','LFO 1','LFO 2'),
        ('Amount','Feedback','Time','Amount','Feedback','Time','Glide','Volume'));
{$endif}

const VirtCC_PADINDEXSTART  = 128;
const PhysCC_Effects0 = 128;
const __MAX_EFFECT = 16; // keep this in sync UDataTypes.MAX_EFFECT
const PhysCC_EffectsMax = PhysCC_Effects0+__MAX_EFFECT-1;

implementation

end.

