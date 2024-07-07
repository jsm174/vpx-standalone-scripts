' Harlem Globetrotters on Tour (Bally 1979)
'
' https://www.ipdb.org/machine.cgi?id=1125


' Uses 7 digits ROM bootleg
' You need both roms: hglbtrtr.zip and hglbtrtb.zip


' TABLE HISTORY
' VPX 10.7 version by JPSalas 2022, version 4.0
' Script based on Gaston's script
' Dedicado a Jolo :)
' DOF extension ny arngrim
'
' Mikcab(MOD) : * Fleep mechanical Sounds
'               * nFozzy physics
'               * Slingshot correction
'               * Targets and Drop Targets physics , add correct switch multipliers drop targets sw1, sw2, sw3, sw4 (ipdb , Bally_1979_Harlem_Globetrotters_On_Tour_Manual.pdf)
'               * Flippers shadow and Dynamic ball shadow
'               * Flippers Bats Primitive 
'
' VR Conversion - TastyWasps, leojreimroc, RajoJoey - April 2023
'
' VPW UPDATE TEAM
'   * mcarter78 - Point Guard (Project Lead, Blender Toolkit, Physics, Sound, Code, VR mega room) 
'   * apophis - Center (Animation, Blender Support, Code)
'   * RetroRitchie - Guard (Blender Support)
'   * tomate - Forward (Blender Support)
'   * bord - Forward (Blender Support)
'   * Clark Kent (Playfield Image)
'   * DaRdog (Cabinet Images, VR Support)
'   * CainArg (Graphics Support)
'   * Niwak for the blender toolkit and parts library



Option Explicit
Randomize
SetLocale 1033	

'*******************************************
'  Constants and Global Variables
'*******************************************
Const BallSize = 50					'Ball size must be 50
Const BallMass = 1					'Ball mass must be 1
Const tnob = 1						'Total number of balls
Const lob = 0						'Locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height


'*******************************************
' VLM  Arrays
'*******************************************

' VLM  Arrays - Start
' Arrays per baked part
Dim BP_Bumper1_Socket: BP_Bumper1_Socket=Array(BM_Bumper1_Socket, LM_L_l42_Bumper1_Socket, LM_L_l26_Bumper1_Socket, LM_L_l42b_Bumper1_Socket, LM_GI_gi002_Bumper1_Socket, LM_GI_gi012_Bumper1_Socket, LM_GI_gi014_Bumper1_Socket, LM_GI_gi015_Bumper1_Socket, LM_GI_gi019_Bumper1_Socket, LM_GI_gi021_Bumper1_Socket, LM_GI_gi023_Bumper1_Socket, LM_GI_gi025_Bumper1_Socket, LM_GI_gi029_Bumper1_Socket, LM_GI_gi031_Bumper1_Socket, LM_GI_gi033_Bumper1_Socket, LM_GI_gi035_Bumper1_Socket, LM_GI_gi042_Bumper1_Socket, LM_GI_gi043_Bumper1_Socket)
Dim BP_Bumper1_ring: BP_Bumper1_ring=Array(BM_Bumper1_ring, LM_L_l42_Bumper1_ring, LM_L_l26_Bumper1_ring, LM_L_l42b_Bumper1_ring, LM_GI_gi012_Bumper1_ring, LM_GI_gi014_Bumper1_ring, LM_GI_gi015_Bumper1_ring, LM_GI_gi019_Bumper1_ring, LM_GI_gi021_Bumper1_ring, LM_GI_gi023_Bumper1_ring, LM_GI_gi025_Bumper1_ring, LM_GI_gi029_Bumper1_ring, LM_GI_gi042_Bumper1_ring, LM_GI_gi043_Bumper1_ring)
Dim BP_Bumper2_Socket: BP_Bumper2_Socket=Array(BM_Bumper2_Socket, LM_L_l42_Bumper2_Socket, LM_L_l26_Bumper2_Socket, LM_L_l42b_Bumper2_Socket, LM_GI_gi003_Bumper2_Socket, LM_GI_gi008_Bumper2_Socket, LM_GI_gi010_Bumper2_Socket, LM_GI_gi012_Bumper2_Socket, LM_GI_gi014_Bumper2_Socket, LM_GI_gi015_Bumper2_Socket, LM_GI_gi017_Bumper2_Socket, LM_GI_gi019_Bumper2_Socket, LM_GI_gi021_Bumper2_Socket, LM_GI_gi025_Bumper2_Socket, LM_GI_gi029_Bumper2_Socket, LM_GI_gi033_Bumper2_Socket, LM_GI_gi037_Bumper2_Socket, LM_GI_gi039_Bumper2_Socket, LM_GI_gi042_Bumper2_Socket, LM_GI_gi043_Bumper2_Socket)
Dim BP_Bumper2_ring: BP_Bumper2_ring=Array(BM_Bumper2_ring, LM_L_l42_Bumper2_ring, LM_L_l26_Bumper2_ring, LM_L_l42b_Bumper2_ring, LM_GI_gi012_Bumper2_ring, LM_GI_gi014_Bumper2_ring, LM_GI_gi015_Bumper2_ring, LM_GI_gi017_Bumper2_ring, LM_GI_gi019_Bumper2_ring, LM_GI_gi021_Bumper2_ring, LM_GI_gi025_Bumper2_ring, LM_GI_gi029_Bumper2_ring, LM_GI_gi039_Bumper2_ring, LM_GI_gi042_Bumper2_ring, LM_GI_gi043_Bumper2_ring)
Dim BP_Bumper3_Socket: BP_Bumper3_Socket=Array(BM_Bumper3_Socket, LM_L_l42_Bumper3_Socket, LM_L_l26_Bumper3_Socket, LM_L_l42b_Bumper3_Socket, LM_GI_gi008_Bumper3_Socket, LM_GI_gi010_Bumper3_Socket, LM_GI_gi014_Bumper3_Socket, LM_GI_gi015_Bumper3_Socket, LM_GI_gi017_Bumper3_Socket, LM_GI_gi019_Bumper3_Socket, LM_GI_gi021_Bumper3_Socket, LM_GI_gi023_Bumper3_Socket, LM_GI_gi025_Bumper3_Socket, LM_GI_gi029_Bumper3_Socket, LM_GI_gi033_Bumper3_Socket, LM_GI_gi035_Bumper3_Socket, LM_GI_gi037_Bumper3_Socket, LM_GI_gi039_Bumper3_Socket, LM_GI_gi042_Bumper3_Socket, LM_GI_gi043_Bumper3_Socket)
Dim BP_Bumper3_ring: BP_Bumper3_ring=Array(BM_Bumper3_ring, LM_L_l42_Bumper3_ring, LM_L_l26_Bumper3_ring, LM_L_l42b_Bumper3_ring, LM_GI_gi019_Bumper3_ring, LM_GI_gi021_Bumper3_ring, LM_GI_gi023_Bumper3_ring, LM_GI_gi029_Bumper3_ring, LM_GI_gi033_Bumper3_ring, LM_GI_gi037_Bumper3_ring, LM_GI_gi039_Bumper3_ring, LM_GI_gi043_Bumper3_ring)
Dim BP_DTsw1: BP_DTsw1=Array(BM_DTsw1, LM_L_l26_DTsw1, LM_L_l42b_DTsw1, LM_GI_gi017_DTsw1, LM_GI_gi019_DTsw1, LM_GI_gi021_DTsw1, LM_GI_gi023_DTsw1, LM_GI_gi035_DTsw1, LM_GI_gi037_DTsw1)
Dim BP_DTsw2: BP_DTsw2=Array(BM_DTsw2, LM_L_l26_DTsw2, LM_L_l42b_DTsw2, LM_GI_gi017_DTsw2, LM_GI_gi019_DTsw2, LM_GI_gi021_DTsw2, LM_GI_gi023_DTsw2, LM_GI_gi035_DTsw2, LM_GI_gi037_DTsw2, LM_GI_gi042_DTsw2)
Dim BP_DTsw3: BP_DTsw3=Array(BM_DTsw3, LM_L_l26_DTsw3, LM_L_l42b_DTsw3, LM_GI_gi015_DTsw3, LM_GI_gi017_DTsw3, LM_GI_gi019_DTsw3, LM_GI_gi023_DTsw3, LM_GI_gi035_DTsw3, LM_GI_gi037_DTsw3, LM_GI_gi042_DTsw3)
Dim BP_DTsw4: BP_DTsw4=Array(BM_DTsw4, LM_L_l26_DTsw4, LM_L_l42b_DTsw4, LM_GI_gi015_DTsw4, LM_GI_gi017_DTsw4, LM_GI_gi019_DTsw4, LM_GI_gi023_DTsw4, LM_GI_gi035_DTsw4, LM_GI_gi037_DTsw4, LM_GI_gi042_DTsw4)
Dim BP_Diverter: BP_Diverter=Array(BM_Diverter, LM_GI_gi003_Diverter, LM_GI_gi005_Diverter, LM_GI_gi039_Diverter)
Dim BP_Gate2: BP_Gate2=Array(BM_Gate2, LM_GI_gi021_Gate2, LM_GI_gi023_Gate2)
Dim BP_Gate3: BP_Gate3=Array(BM_Gate3, LM_L_l42_Gate3, LM_GI_gi029_Gate3, LM_GI_gi035_Gate3, LM_GI_gi037_Gate3, LM_GI_gi043_Gate3)
Dim BP_Gate4: BP_Gate4=Array(BM_Gate4, LM_L_l42_Gate4, LM_GI_gi029_Gate4, LM_GI_gi031_Gate4, LM_GI_gi033_Gate4, LM_GI_gi043_Gate4)
Dim BP_InstructionCard2: BP_InstructionCard2=Array(BM_InstructionCard2)
Dim BP_InsturctionCard1: BP_InsturctionCard1=Array(BM_InsturctionCard1)
Dim BP_LFlipper: BP_LFlipper=Array(BM_LFlipper, LM_GI_gi002_LFlipper, LM_GI_gi003_LFlipper)
Dim BP_LFlipper1: BP_LFlipper1=Array(BM_LFlipper1, LM_GI_gi002_LFlipper1, LM_GI_gi003_LFlipper1)
Dim BP_LFlipper1U: BP_LFlipper1U=Array(BM_LFlipper1U, LM_GI_gi002_LFlipper1U, LM_GI_gi003_LFlipper1U)
Dim BP_LFlipperU: BP_LFlipperU=Array(BM_LFlipperU, LM_GI_gi002_LFlipperU, LM_GI_gi003_LFlipperU, LM_L_l1_LFlipperU, LM_L_l17_LFlipperU)
Dim BP_Layer1: BP_Layer1=Array(BM_Layer1, LM_L_l42_Layer1, LM_L_l26_Layer1, LM_L_l42b_Layer1, LM_GI_gi002_Layer1, LM_GI_gi003_Layer1, LM_GI_gi005_Layer1, LM_GI_gi008_Layer1, LM_GI_gi010_Layer1, LM_GI_gi012_Layer1, LM_GI_gi014_Layer1, LM_GI_gi015_Layer1, LM_GI_gi017_Layer1, LM_GI_gi019_Layer1, LM_GI_gi021_Layer1, LM_GI_gi023_Layer1, LM_GI_gi025_Layer1, LM_GI_gi029_Layer1, LM_GI_gi031_Layer1, LM_GI_gi033_Layer1, LM_GI_gi035_Layer1, LM_GI_gi037_Layer1, LM_GI_gi039_Layer1, LM_GI_gi042_Layer1, LM_GI_gi043_Layer1, LM_L_l19_Layer1, LM_L_l23_Layer1, LM_L_l39_Layer1, LM_L_l53_Layer1)
Dim BP_LeftSling1: BP_LeftSling1=Array(BM_LeftSling1, LM_GI_gi002_LeftSling1, LM_GI_gi003_LeftSling1, LM_GI_gi008_LeftSling1, LM_GI_gi039_LeftSling1, LM_L_l40_LeftSling1)
Dim BP_LeftSling2: BP_LeftSling2=Array(BM_LeftSling2, LM_L_l26_LeftSling2, LM_GI_gi002_LeftSling2, LM_GI_gi003_LeftSling2, LM_GI_gi008_LeftSling2, LM_GI_gi039_LeftSling2)
Dim BP_LeftSling3: BP_LeftSling3=Array(BM_LeftSling3, LM_L_l26_LeftSling3, LM_GI_gi002_LeftSling3, LM_GI_gi003_LeftSling3, LM_GI_gi005_LeftSling3, LM_GI_gi008_LeftSling3, LM_GI_gi039_LeftSling3)
Dim BP_LeftSling4: BP_LeftSling4=Array(BM_LeftSling4, LM_L_l26_LeftSling4, LM_GI_gi002_LeftSling4, LM_GI_gi003_LeftSling4, LM_GI_gi005_LeftSling4, LM_GI_gi008_LeftSling4, LM_GI_gi039_LeftSling4)
Dim BP_Parts: BP_Parts=Array(BM_Parts, LM_L_l42_Parts, LM_L_l26_Parts, LM_L_l42b_Parts, LM_GI_gi002_Parts, LM_GI_gi003_Parts, LM_GI_gi005_Parts, LM_GI_gi008_Parts, LM_GI_gi010_Parts, LM_GI_gi012_Parts, LM_GI_gi014_Parts, LM_GI_gi015_Parts, LM_GI_gi017_Parts, LM_GI_gi019_Parts, LM_GI_gi021_Parts, LM_GI_gi023_Parts, LM_GI_gi025_Parts, LM_GI_gi029_Parts, LM_GI_gi031_Parts, LM_GI_gi033_Parts, LM_GI_gi035_Parts, LM_GI_gi037_Parts, LM_GI_gi039_Parts, LM_GI_gi042_Parts, LM_GI_gi043_Parts, LM_L_l10_Parts, LM_L_l19_Parts, LM_L_l21_Parts, LM_L_l23_Parts, LM_L_l25_Parts, LM_L_l35_Parts, LM_L_l37_Parts, LM_L_l38_Parts, LM_L_l39_Parts, LM_L_l40_Parts, LM_L_l41_Parts, LM_L_l5_Parts, LM_L_l500_Parts, LM_L_l51_Parts, LM_L_l53_Parts, LM_L_l54_Parts, LM_L_l55_Parts, LM_L_l57_Parts, LM_L_l58_Parts, LM_L_l59_Parts, LM_L_l6_Parts, LM_L_l7_Parts, LM_L_l8_Parts, LM_L_l9_Parts)
Dim BP_PincabRails: BP_PincabRails=Array(BM_PincabRails, LM_L_l42_PincabRails, LM_L_l26_PincabRails, LM_L_l42b_PincabRails, LM_GI_gi017_PincabRails, LM_GI_gi025_PincabRails, LM_GI_gi043_PincabRails)
Dim BP_Playfield: BP_Playfield=Array(BM_Playfield, LM_L_l42_Playfield, LM_L_l26_Playfield, LM_L_l42b_Playfield, LM_GI_gi002_Playfield, LM_GI_gi003_Playfield, LM_GI_gi005_Playfield, LM_GI_gi008_Playfield, LM_GI_gi010_Playfield, LM_GI_gi012_Playfield, LM_GI_gi014_Playfield, LM_GI_gi015_Playfield, LM_GI_gi017_Playfield, LM_GI_gi019_Playfield, LM_GI_gi021_Playfield, LM_GI_gi023_Playfield, LM_GI_gi025_Playfield, LM_GI_gi029_Playfield, LM_GI_gi031_Playfield, LM_GI_gi033_Playfield, LM_GI_gi035_Playfield, LM_GI_gi037_Playfield, LM_GI_gi039_Playfield, LM_GI_gi042_Playfield, LM_GI_gi043_Playfield, LM_L_l1_Playfield, LM_L_l10_Playfield, LM_L_l11_Playfield, LM_L_l12_Playfield, LM_L_l14_Playfield, LM_L_l15_Playfield, LM_L_l17_Playfield, LM_L_l18_Playfield, LM_L_l19_Playfield, LM_L_l2_Playfield, LM_L_l20_Playfield, LM_L_l200_Playfield, LM_L_l21_Playfield, LM_L_l22_Playfield, LM_L_l23_Playfield, LM_L_l24_Playfield, LM_L_l25_Playfield, LM_L_l28_Playfield, LM_L_l3_Playfield, LM_L_l30_Playfield, LM_L_l31_Playfield, _
	LM_L_l33_Playfield, LM_L_l34_Playfield, LM_L_l35_Playfield, LM_L_l36_Playfield, LM_L_l360_Playfield, LM_L_l37_Playfield, LM_L_l38_Playfield, LM_L_l39_Playfield, LM_L_l4_Playfield, LM_L_l40_Playfield, LM_L_l41_Playfield, LM_L_l43_Playfield, LM_L_l44_Playfield, LM_L_l46_Playfield, LM_L_l47_Playfield, LM_L_l49_Playfield, LM_L_l5_Playfield, LM_L_l50_Playfield, LM_L_l500_Playfield, LM_L_l51_Playfield, LM_L_l52_Playfield, LM_L_l520_Playfield, LM_L_l53_Playfield, LM_L_l54_Playfield, LM_L_l55_Playfield, LM_L_l56_Playfield, LM_L_l57_Playfield, LM_L_l6_Playfield, LM_L_l60_Playfield, LM_L_l62_Playfield, LM_L_l63_Playfield, LM_L_l7_Playfield, LM_L_l8_Playfield, LM_L_l9_Playfield)
Dim BP_RFlipper: BP_RFlipper=Array(BM_RFlipper, LM_GI_gi002_RFlipper, LM_GI_gi003_RFlipper, LM_GI_gi039_RFlipper)
Dim BP_RFlipperU: BP_RFlipperU=Array(BM_RFlipperU, LM_GI_gi002_RFlipperU, LM_GI_gi003_RFlipperU, LM_GI_gi039_RFlipperU, LM_L_l1_RFlipperU, LM_L_l33_RFlipperU)
Dim BP_Ramp1: BP_Ramp1=Array(BM_Ramp1, LM_L_l42_Ramp1, LM_L_l26_Ramp1, LM_L_l42b_Ramp1, LM_GI_gi002_Ramp1, LM_GI_gi003_Ramp1, LM_GI_gi005_Ramp1, LM_GI_gi014_Ramp1, LM_GI_gi015_Ramp1, LM_GI_gi017_Ramp1, LM_GI_gi019_Ramp1, LM_GI_gi021_Ramp1, LM_GI_gi023_Ramp1, LM_GI_gi025_Ramp1, LM_GI_gi035_Ramp1, LM_GI_gi037_Ramp1, LM_GI_gi039_Ramp1, LM_GI_gi042_Ramp1)
Dim BP_Ramp2: BP_Ramp2=Array(BM_Ramp2, LM_L_l42_Ramp2, LM_L_l26_Ramp2, LM_L_l42b_Ramp2, LM_GI_gi002_Ramp2, LM_GI_gi003_Ramp2, LM_GI_gi005_Ramp2, LM_GI_gi008_Ramp2, LM_GI_gi010_Ramp2, LM_GI_gi012_Ramp2, LM_GI_gi014_Ramp2, LM_GI_gi015_Ramp2, LM_GI_gi017_Ramp2, LM_GI_gi019_Ramp2, LM_GI_gi021_Ramp2, LM_GI_gi025_Ramp2, LM_GI_gi029_Ramp2, LM_GI_gi031_Ramp2, LM_GI_gi033_Ramp2, LM_GI_gi039_Ramp2, LM_GI_gi042_Ramp2, LM_GI_gi043_Ramp2, LM_L_l40_Ramp2)
Dim BP_RightSling1: BP_RightSling1=Array(BM_RightSling1, LM_GI_gi002_RightSling1, LM_GI_gi003_RightSling1, LM_GI_gi005_RightSling1, LM_GI_gi015_RightSling1, LM_GI_gi039_RightSling1)
Dim BP_RightSling2: BP_RightSling2=Array(BM_RightSling2, LM_L_l26_RightSling2, LM_GI_gi002_RightSling2, LM_GI_gi003_RightSling2, LM_GI_gi005_RightSling2, LM_GI_gi039_RightSling2)
Dim BP_RightSling3: BP_RightSling3=Array(BM_RightSling3, LM_L_l26_RightSling3, LM_GI_gi002_RightSling3, LM_GI_gi003_RightSling3, LM_GI_gi005_RightSling3, LM_GI_gi039_RightSling3)
Dim BP_RightSling4: BP_RightSling4=Array(BM_RightSling4, LM_L_l26_RightSling4, LM_GI_gi002_RightSling4, LM_GI_gi003_RightSling4, LM_GI_gi005_RightSling4, LM_GI_gi039_RightSling4)
Dim BP_Rollover_sw22: BP_Rollover_sw22=Array(BM_Rollover_sw22, LM_GI_gi003_Rollover_sw22, LM_GI_gi039_Rollover_sw22)
Dim BP_Rollover_sw23: BP_Rollover_sw23=Array(BM_Rollover_sw23, LM_GI_gi039_Rollover_sw23)
Dim BP_Rollover_sw31: BP_Rollover_sw31=Array(BM_Rollover_sw31, LM_GI_gi002_Rollover_sw31)
Dim BP_STsw26: BP_STsw26=Array(BM_STsw26, LM_GI_gi008_STsw26, LM_GI_gi010_STsw26)
Dim BP_STsw27: BP_STsw27=Array(BM_STsw27, LM_L_l26_STsw27, LM_GI_gi008_STsw27, LM_GI_gi010_STsw27, LM_GI_gi012_STsw27)
Dim BP_STsw28: BP_STsw28=Array(BM_STsw28, LM_L_l42_STsw28, LM_L_l26_STsw28, LM_GI_gi008_STsw28, LM_GI_gi010_STsw28, LM_GI_gi012_STsw28)
Dim BP_STsw29: BP_STsw29=Array(BM_STsw29, LM_L_l42_STsw29, LM_L_l26_STsw29, LM_GI_gi008_STsw29, LM_GI_gi010_STsw29, LM_GI_gi012_STsw29, LM_GI_gi025_STsw29)
Dim BP_STsw30: BP_STsw30=Array(BM_STsw30, LM_L_l42_STsw30, LM_L_l26_STsw30, LM_GI_gi008_STsw30, LM_GI_gi010_STsw30, LM_GI_gi012_STsw30, LM_GI_gi025_STsw30)
Dim BP_STsw35: BP_STsw35=Array(BM_STsw35, LM_L_l26_STsw35, LM_GI_gi005_STsw35, LM_GI_gi015_STsw35, LM_GI_gi017_STsw35, LM_GI_gi019_STsw35, LM_GI_gi023_STsw35, LM_GI_gi042_STsw35)
Dim BP_SlingArm1: BP_SlingArm1=Array(BM_SlingArm1, LM_GI_gi002_SlingArm1, LM_GI_gi003_SlingArm1, LM_GI_gi005_SlingArm1, LM_GI_gi039_SlingArm1)
Dim BP_SlingArm2: BP_SlingArm2=Array(BM_SlingArm2, LM_GI_gi002_SlingArm2, LM_GI_gi039_SlingArm2)
Dim BP_Spinner_sw17A: BP_Spinner_sw17A=Array(BM_Spinner_sw17A, LM_L_l42_Spinner_sw17A, LM_L_l26_Spinner_sw17A, LM_L_l42b_Spinner_sw17A, LM_GI_gi008_Spinner_sw17A, LM_GI_gi010_Spinner_sw17A, LM_GI_gi012_Spinner_sw17A, LM_GI_gi014_Spinner_sw17A, LM_GI_gi015_Spinner_sw17A, LM_GI_gi025_Spinner_sw17A, LM_GI_gi029_Spinner_sw17A, LM_GI_gi031_Spinner_sw17A, LM_GI_gi043_Spinner_sw17A, LM_L_l38_Spinner_sw17A, LM_L_l54_Spinner_sw17A)
Dim BP_Spinner_sw17B: BP_Spinner_sw17B=Array(BM_Spinner_sw17B, LM_L_l42_Spinner_sw17B, LM_L_l26_Spinner_sw17B, LM_L_l42b_Spinner_sw17B, LM_GI_gi008_Spinner_sw17B, LM_GI_gi010_Spinner_sw17B, LM_GI_gi012_Spinner_sw17B, LM_GI_gi014_Spinner_sw17B, LM_GI_gi015_Spinner_sw17B, LM_GI_gi025_Spinner_sw17B, LM_GI_gi029_Spinner_sw17B, LM_GI_gi031_Spinner_sw17B, LM_GI_gi043_Spinner_sw17B, LM_L_l38_Spinner_sw17B, LM_L_l54_Spinner_sw17B)
Dim BP_Spinner_sw17W: BP_Spinner_sw17W=Array(BM_Spinner_sw17W, LM_L_l42_Spinner_sw17W, LM_GI_gi012_Spinner_sw17W, LM_GI_gi014_Spinner_sw17W, LM_GI_gi025_Spinner_sw17W, LM_GI_gi043_Spinner_sw17W, LM_L_l54_Spinner_sw17W)
Dim BP_Spinner_sw17_Wire: BP_Spinner_sw17_Wire=Array(BM_Spinner_sw17_Wire, LM_L_l42_Spinner_sw17_Wire, LM_L_l26_Spinner_sw17_Wire, LM_GI_gi012_Spinner_sw17_Wire, LM_GI_gi014_Spinner_sw17_Wire, LM_GI_gi025_Spinner_sw17_Wire, LM_GI_gi043_Spinner_sw17_Wire, LM_L_l54_Spinner_sw17_Wire)
Dim BP_Spinner_sw25A: BP_Spinner_sw25A=Array(BM_Spinner_sw25A, LM_L_l42_Spinner_sw25A, LM_L_l26_Spinner_sw25A, LM_L_l42b_Spinner_sw25A, LM_GI_gi005_Spinner_sw25A, LM_GI_gi014_Spinner_sw25A, LM_GI_gi015_Spinner_sw25A, LM_GI_gi019_Spinner_sw25A, LM_GI_gi021_Spinner_sw25A, LM_GI_gi023_Spinner_sw25A, LM_GI_gi025_Spinner_sw25A, LM_GI_gi029_Spinner_sw25A, LM_GI_gi037_Spinner_sw25A, LM_GI_gi042_Spinner_sw25A, LM_GI_gi043_Spinner_sw25A, LM_L_l35_Spinner_sw25A, LM_L_l500_Spinner_sw25A)
Dim BP_Spinner_sw25B: BP_Spinner_sw25B=Array(BM_Spinner_sw25B, LM_L_l42_Spinner_sw25B, LM_L_l26_Spinner_sw25B, LM_L_l42b_Spinner_sw25B, LM_GI_gi005_Spinner_sw25B, LM_GI_gi014_Spinner_sw25B, LM_GI_gi015_Spinner_sw25B, LM_GI_gi019_Spinner_sw25B, LM_GI_gi021_Spinner_sw25B, LM_GI_gi023_Spinner_sw25B, LM_GI_gi025_Spinner_sw25B, LM_GI_gi029_Spinner_sw25B, LM_GI_gi037_Spinner_sw25B, LM_GI_gi042_Spinner_sw25B, LM_GI_gi043_Spinner_sw25B, LM_L_l35_Spinner_sw25B, LM_L_l500_Spinner_sw25B)
Dim BP_Spinner_sw25W: BP_Spinner_sw25W=Array(BM_Spinner_sw25W, LM_L_l26_Spinner_sw25W, LM_L_l42b_Spinner_sw25W)
Dim BP_Spinner_sw25_Wire: BP_Spinner_sw25_Wire=Array(BM_Spinner_sw25_Wire, LM_L_l42_Spinner_sw25_Wire, LM_L_l26_Spinner_sw25_Wire, LM_L_l42b_Spinner_sw25_Wire, LM_GI_gi005_Spinner_sw25_Wire, LM_GI_gi014_Spinner_sw25_Wire, LM_GI_gi015_Spinner_sw25_Wire, LM_GI_gi019_Spinner_sw25_Wire, LM_GI_gi021_Spinner_sw25_Wire, LM_GI_gi025_Spinner_sw25_Wire)
Dim BP_Spinner_sw33A: BP_Spinner_sw33A=Array(BM_Spinner_sw33A, LM_L_l42_Spinner_sw33A, LM_L_l26_Spinner_sw33A, LM_L_l42b_Spinner_sw33A, LM_GI_gi008_Spinner_sw33A, LM_GI_gi014_Spinner_sw33A, LM_GI_gi015_Spinner_sw33A, LM_GI_gi019_Spinner_sw33A, LM_GI_gi021_Spinner_sw33A, LM_GI_gi023_Spinner_sw33A, LM_GI_gi025_Spinner_sw33A, LM_GI_gi029_Spinner_sw33A, LM_GI_gi042_Spinner_sw33A, LM_GI_gi043_Spinner_sw33A, LM_L_l5_Spinner_sw33A, LM_L_l51_Spinner_sw33A)
Dim BP_Spinner_sw33B: BP_Spinner_sw33B=Array(BM_Spinner_sw33B, LM_L_l42_Spinner_sw33B, LM_L_l26_Spinner_sw33B, LM_L_l42b_Spinner_sw33B, LM_GI_gi008_Spinner_sw33B, LM_GI_gi014_Spinner_sw33B, LM_GI_gi015_Spinner_sw33B, LM_GI_gi019_Spinner_sw33B, LM_GI_gi021_Spinner_sw33B, LM_GI_gi023_Spinner_sw33B, LM_GI_gi025_Spinner_sw33B, LM_GI_gi029_Spinner_sw33B, LM_GI_gi042_Spinner_sw33B, LM_GI_gi043_Spinner_sw33B, LM_L_l5_Spinner_sw33B, LM_L_l51_Spinner_sw33B)
Dim BP_Spinner_sw33W: BP_Spinner_sw33W=Array(BM_Spinner_sw33W, LM_L_l42_Spinner_sw33W, LM_L_l26_Spinner_sw33W)
Dim BP_Spinner_sw33_Wire: BP_Spinner_sw33_Wire=Array(BM_Spinner_sw33_Wire, LM_L_l42_Spinner_sw33_Wire, LM_L_l26_Spinner_sw33_Wire, LM_L_l42b_Spinner_sw33_Wire, LM_GI_gi014_Spinner_sw33_Wire, LM_GI_gi015_Spinner_sw33_Wire, LM_GI_gi019_Spinner_sw33_Wire, LM_GI_gi021_Spinner_sw33_Wire, LM_GI_gi025_Spinner_sw33_Wire)
Dim BP_apron1: BP_apron1=Array(BM_apron1, LM_GI_gi002_apron1, LM_GI_gi003_apron1, LM_GI_gi039_apron1, LM_L_l59_apron1)
Dim BP_hg_apron2_001: BP_hg_apron2_001=Array(BM_hg_apron2_001)
Dim BP_hg_apron2_002: BP_hg_apron2_002=Array(BM_hg_apron2_002)
Dim BP_hg_apron2_003: BP_hg_apron2_003=Array(BM_hg_apron2_003, LM_GI_gi002_hg_apron2_003, LM_L_l59_hg_apron2_003)
Dim BP_hg_apron2_004: BP_hg_apron2_004=Array(BM_hg_apron2_004)
' Arrays per lighting scenario
Dim BL_GI_gi002: BL_GI_gi002=Array(LM_GI_gi002_Bumper1_Socket, LM_GI_gi002_LFlipper, LM_GI_gi002_LFlipper1, LM_GI_gi002_LFlipper1U, LM_GI_gi002_LFlipperU, LM_GI_gi002_Layer1, LM_GI_gi002_LeftSling1, LM_GI_gi002_LeftSling2, LM_GI_gi002_LeftSling3, LM_GI_gi002_LeftSling4, LM_GI_gi002_Parts, LM_GI_gi002_Playfield, LM_GI_gi002_RFlipper, LM_GI_gi002_RFlipperU, LM_GI_gi002_Ramp1, LM_GI_gi002_Ramp2, LM_GI_gi002_RightSling1, LM_GI_gi002_RightSling2, LM_GI_gi002_RightSling3, LM_GI_gi002_RightSling4, LM_GI_gi002_Rollover_sw31, LM_GI_gi002_SlingArm1, LM_GI_gi002_SlingArm2, LM_GI_gi002_apron1, LM_GI_gi002_hg_apron2_003)
Dim BL_GI_gi003: BL_GI_gi003=Array(LM_GI_gi003_Bumper2_Socket, LM_GI_gi003_Diverter, LM_GI_gi003_LFlipper, LM_GI_gi003_LFlipper1, LM_GI_gi003_LFlipper1U, LM_GI_gi003_LFlipperU, LM_GI_gi003_Layer1, LM_GI_gi003_LeftSling1, LM_GI_gi003_LeftSling2, LM_GI_gi003_LeftSling3, LM_GI_gi003_LeftSling4, LM_GI_gi003_Parts, LM_GI_gi003_Playfield, LM_GI_gi003_RFlipper, LM_GI_gi003_RFlipperU, LM_GI_gi003_Ramp1, LM_GI_gi003_Ramp2, LM_GI_gi003_RightSling1, LM_GI_gi003_RightSling2, LM_GI_gi003_RightSling3, LM_GI_gi003_RightSling4, LM_GI_gi003_Rollover_sw22, LM_GI_gi003_SlingArm1, LM_GI_gi003_apron1)
Dim BL_GI_gi005: BL_GI_gi005=Array(LM_GI_gi005_Diverter, LM_GI_gi005_Layer1, LM_GI_gi005_LeftSling3, LM_GI_gi005_LeftSling4, LM_GI_gi005_Parts, LM_GI_gi005_Playfield, LM_GI_gi005_Ramp1, LM_GI_gi005_Ramp2, LM_GI_gi005_RightSling1, LM_GI_gi005_RightSling2, LM_GI_gi005_RightSling3, LM_GI_gi005_RightSling4, LM_GI_gi005_STsw35, LM_GI_gi005_SlingArm1, LM_GI_gi005_Spinner_sw25A, LM_GI_gi005_Spinner_sw25B, LM_GI_gi005_Spinner_sw25_Wire)
Dim BL_GI_gi008: BL_GI_gi008=Array(LM_GI_gi008_Bumper2_Socket, LM_GI_gi008_Bumper3_Socket, LM_GI_gi008_Layer1, LM_GI_gi008_LeftSling1, LM_GI_gi008_LeftSling2, LM_GI_gi008_LeftSling3, LM_GI_gi008_LeftSling4, LM_GI_gi008_Parts, LM_GI_gi008_Playfield, LM_GI_gi008_Ramp2, LM_GI_gi008_STsw26, LM_GI_gi008_STsw27, LM_GI_gi008_STsw28, LM_GI_gi008_STsw29, LM_GI_gi008_STsw30, LM_GI_gi008_Spinner_sw17A, LM_GI_gi008_Spinner_sw17B, LM_GI_gi008_Spinner_sw33A, LM_GI_gi008_Spinner_sw33B)
Dim BL_GI_gi010: BL_GI_gi010=Array(LM_GI_gi010_Bumper2_Socket, LM_GI_gi010_Bumper3_Socket, LM_GI_gi010_Layer1, LM_GI_gi010_Parts, LM_GI_gi010_Playfield, LM_GI_gi010_Ramp2, LM_GI_gi010_STsw26, LM_GI_gi010_STsw27, LM_GI_gi010_STsw28, LM_GI_gi010_STsw29, LM_GI_gi010_STsw30, LM_GI_gi010_Spinner_sw17A, LM_GI_gi010_Spinner_sw17B)
Dim BL_GI_gi012: BL_GI_gi012=Array(LM_GI_gi012_Bumper1_Socket, LM_GI_gi012_Bumper1_ring, LM_GI_gi012_Bumper2_Socket, LM_GI_gi012_Bumper2_ring, LM_GI_gi012_Layer1, LM_GI_gi012_Parts, LM_GI_gi012_Playfield, LM_GI_gi012_Ramp2, LM_GI_gi012_STsw27, LM_GI_gi012_STsw28, LM_GI_gi012_STsw29, LM_GI_gi012_STsw30, LM_GI_gi012_Spinner_sw17A, LM_GI_gi012_Spinner_sw17B, LM_GI_gi012_Spinner_sw17W, LM_GI_gi012_Spinner_sw17_Wire)
Dim BL_GI_gi014: BL_GI_gi014=Array(LM_GI_gi014_Bumper1_Socket, LM_GI_gi014_Bumper1_ring, LM_GI_gi014_Bumper2_Socket, LM_GI_gi014_Bumper2_ring, LM_GI_gi014_Bumper3_Socket, LM_GI_gi014_Layer1, LM_GI_gi014_Parts, LM_GI_gi014_Playfield, LM_GI_gi014_Ramp1, LM_GI_gi014_Ramp2, LM_GI_gi014_Spinner_sw17A, LM_GI_gi014_Spinner_sw17B, LM_GI_gi014_Spinner_sw17W, LM_GI_gi014_Spinner_sw17_Wire, LM_GI_gi014_Spinner_sw25A, LM_GI_gi014_Spinner_sw25B, LM_GI_gi014_Spinner_sw25_Wire, LM_GI_gi014_Spinner_sw33A, LM_GI_gi014_Spinner_sw33B, LM_GI_gi014_Spinner_sw33_Wire)
Dim BL_GI_gi015: BL_GI_gi015=Array(LM_GI_gi015_Bumper1_Socket, LM_GI_gi015_Bumper1_ring, LM_GI_gi015_Bumper2_Socket, LM_GI_gi015_Bumper2_ring, LM_GI_gi015_Bumper3_Socket, LM_GI_gi015_DTsw3, LM_GI_gi015_DTsw4, LM_GI_gi015_Layer1, LM_GI_gi015_Parts, LM_GI_gi015_Playfield, LM_GI_gi015_Ramp1, LM_GI_gi015_Ramp2, LM_GI_gi015_RightSling1, LM_GI_gi015_STsw35, LM_GI_gi015_Spinner_sw17A, LM_GI_gi015_Spinner_sw17B, LM_GI_gi015_Spinner_sw25A, LM_GI_gi015_Spinner_sw25B, LM_GI_gi015_Spinner_sw25_Wire, LM_GI_gi015_Spinner_sw33A, LM_GI_gi015_Spinner_sw33B, LM_GI_gi015_Spinner_sw33_Wire)
Dim BL_GI_gi017: BL_GI_gi017=Array(LM_GI_gi017_Bumper2_Socket, LM_GI_gi017_Bumper2_ring, LM_GI_gi017_Bumper3_Socket, LM_GI_gi017_DTsw1, LM_GI_gi017_DTsw2, LM_GI_gi017_DTsw3, LM_GI_gi017_DTsw4, LM_GI_gi017_Layer1, LM_GI_gi017_Parts, LM_GI_gi017_PincabRails, LM_GI_gi017_Playfield, LM_GI_gi017_Ramp1, LM_GI_gi017_Ramp2, LM_GI_gi017_STsw35)
Dim BL_GI_gi019: BL_GI_gi019=Array(LM_GI_gi019_Bumper1_Socket, LM_GI_gi019_Bumper1_ring, LM_GI_gi019_Bumper2_Socket, LM_GI_gi019_Bumper2_ring, LM_GI_gi019_Bumper3_Socket, LM_GI_gi019_Bumper3_ring, LM_GI_gi019_DTsw1, LM_GI_gi019_DTsw2, LM_GI_gi019_DTsw3, LM_GI_gi019_DTsw4, LM_GI_gi019_Layer1, LM_GI_gi019_Parts, LM_GI_gi019_Playfield, LM_GI_gi019_Ramp1, LM_GI_gi019_Ramp2, LM_GI_gi019_STsw35, LM_GI_gi019_Spinner_sw25A, LM_GI_gi019_Spinner_sw25B, LM_GI_gi019_Spinner_sw25_Wire, LM_GI_gi019_Spinner_sw33A, LM_GI_gi019_Spinner_sw33B, LM_GI_gi019_Spinner_sw33_Wire)
Dim BL_GI_gi021: BL_GI_gi021=Array(LM_GI_gi021_Bumper1_Socket, LM_GI_gi021_Bumper1_ring, LM_GI_gi021_Bumper2_Socket, LM_GI_gi021_Bumper2_ring, LM_GI_gi021_Bumper3_Socket, LM_GI_gi021_Bumper3_ring, LM_GI_gi021_DTsw1, LM_GI_gi021_DTsw2, LM_GI_gi021_Gate2, LM_GI_gi021_Layer1, LM_GI_gi021_Parts, LM_GI_gi021_Playfield, LM_GI_gi021_Ramp1, LM_GI_gi021_Ramp2, LM_GI_gi021_Spinner_sw25A, LM_GI_gi021_Spinner_sw25B, LM_GI_gi021_Spinner_sw25_Wire, LM_GI_gi021_Spinner_sw33A, LM_GI_gi021_Spinner_sw33B, LM_GI_gi021_Spinner_sw33_Wire)
Dim BL_GI_gi023: BL_GI_gi023=Array(LM_GI_gi023_Bumper1_Socket, LM_GI_gi023_Bumper1_ring, LM_GI_gi023_Bumper3_Socket, LM_GI_gi023_Bumper3_ring, LM_GI_gi023_DTsw1, LM_GI_gi023_DTsw2, LM_GI_gi023_DTsw3, LM_GI_gi023_DTsw4, LM_GI_gi023_Gate2, LM_GI_gi023_Layer1, LM_GI_gi023_Parts, LM_GI_gi023_Playfield, LM_GI_gi023_Ramp1, LM_GI_gi023_STsw35, LM_GI_gi023_Spinner_sw25A, LM_GI_gi023_Spinner_sw25B, LM_GI_gi023_Spinner_sw33A, LM_GI_gi023_Spinner_sw33B)
Dim BL_GI_gi025: BL_GI_gi025=Array(LM_GI_gi025_Bumper1_Socket, LM_GI_gi025_Bumper1_ring, LM_GI_gi025_Bumper2_Socket, LM_GI_gi025_Bumper2_ring, LM_GI_gi025_Bumper3_Socket, LM_GI_gi025_Layer1, LM_GI_gi025_Parts, LM_GI_gi025_PincabRails, LM_GI_gi025_Playfield, LM_GI_gi025_Ramp1, LM_GI_gi025_Ramp2, LM_GI_gi025_STsw29, LM_GI_gi025_STsw30, LM_GI_gi025_Spinner_sw17A, LM_GI_gi025_Spinner_sw17B, LM_GI_gi025_Spinner_sw17W, LM_GI_gi025_Spinner_sw17_Wire, LM_GI_gi025_Spinner_sw25A, LM_GI_gi025_Spinner_sw25B, LM_GI_gi025_Spinner_sw25_Wire, LM_GI_gi025_Spinner_sw33A, LM_GI_gi025_Spinner_sw33B, LM_GI_gi025_Spinner_sw33_Wire)
Dim BL_GI_gi029: BL_GI_gi029=Array(LM_GI_gi029_Bumper1_Socket, LM_GI_gi029_Bumper1_ring, LM_GI_gi029_Bumper2_Socket, LM_GI_gi029_Bumper2_ring, LM_GI_gi029_Bumper3_Socket, LM_GI_gi029_Bumper3_ring, LM_GI_gi029_Gate3, LM_GI_gi029_Gate4, LM_GI_gi029_Layer1, LM_GI_gi029_Parts, LM_GI_gi029_Playfield, LM_GI_gi029_Ramp2, LM_GI_gi029_Spinner_sw17A, LM_GI_gi029_Spinner_sw17B, LM_GI_gi029_Spinner_sw25A, LM_GI_gi029_Spinner_sw25B, LM_GI_gi029_Spinner_sw33A, LM_GI_gi029_Spinner_sw33B)
Dim BL_GI_gi031: BL_GI_gi031=Array(LM_GI_gi031_Bumper1_Socket, LM_GI_gi031_Gate4, LM_GI_gi031_Layer1, LM_GI_gi031_Parts, LM_GI_gi031_Playfield, LM_GI_gi031_Ramp2, LM_GI_gi031_Spinner_sw17A, LM_GI_gi031_Spinner_sw17B)
Dim BL_GI_gi033: BL_GI_gi033=Array(LM_GI_gi033_Bumper1_Socket, LM_GI_gi033_Bumper2_Socket, LM_GI_gi033_Bumper3_Socket, LM_GI_gi033_Bumper3_ring, LM_GI_gi033_Gate4, LM_GI_gi033_Layer1, LM_GI_gi033_Parts, LM_GI_gi033_Playfield, LM_GI_gi033_Ramp2)
Dim BL_GI_gi035: BL_GI_gi035=Array(LM_GI_gi035_Bumper1_Socket, LM_GI_gi035_Bumper3_Socket, LM_GI_gi035_DTsw1, LM_GI_gi035_DTsw2, LM_GI_gi035_DTsw3, LM_GI_gi035_DTsw4, LM_GI_gi035_Gate3, LM_GI_gi035_Layer1, LM_GI_gi035_Parts, LM_GI_gi035_Playfield, LM_GI_gi035_Ramp1)
Dim BL_GI_gi037: BL_GI_gi037=Array(LM_GI_gi037_Bumper2_Socket, LM_GI_gi037_Bumper3_Socket, LM_GI_gi037_Bumper3_ring, LM_GI_gi037_DTsw1, LM_GI_gi037_DTsw2, LM_GI_gi037_DTsw3, LM_GI_gi037_DTsw4, LM_GI_gi037_Gate3, LM_GI_gi037_Layer1, LM_GI_gi037_Parts, LM_GI_gi037_Playfield, LM_GI_gi037_Ramp1, LM_GI_gi037_Spinner_sw25A, LM_GI_gi037_Spinner_sw25B)
Dim BL_GI_gi039: BL_GI_gi039=Array(LM_GI_gi039_Bumper2_Socket, LM_GI_gi039_Bumper2_ring, LM_GI_gi039_Bumper3_Socket, LM_GI_gi039_Bumper3_ring, LM_GI_gi039_Diverter, LM_GI_gi039_Layer1, LM_GI_gi039_LeftSling1, LM_GI_gi039_LeftSling2, LM_GI_gi039_LeftSling3, LM_GI_gi039_LeftSling4, LM_GI_gi039_Parts, LM_GI_gi039_Playfield, LM_GI_gi039_RFlipper, LM_GI_gi039_RFlipperU, LM_GI_gi039_Ramp1, LM_GI_gi039_Ramp2, LM_GI_gi039_RightSling1, LM_GI_gi039_RightSling2, LM_GI_gi039_RightSling3, LM_GI_gi039_RightSling4, LM_GI_gi039_Rollover_sw22, LM_GI_gi039_Rollover_sw23, LM_GI_gi039_SlingArm1, LM_GI_gi039_SlingArm2, LM_GI_gi039_apron1)
Dim BL_GI_gi042: BL_GI_gi042=Array(LM_GI_gi042_Bumper1_Socket, LM_GI_gi042_Bumper1_ring, LM_GI_gi042_Bumper2_Socket, LM_GI_gi042_Bumper2_ring, LM_GI_gi042_Bumper3_Socket, LM_GI_gi042_DTsw2, LM_GI_gi042_DTsw3, LM_GI_gi042_DTsw4, LM_GI_gi042_Layer1, LM_GI_gi042_Parts, LM_GI_gi042_Playfield, LM_GI_gi042_Ramp1, LM_GI_gi042_Ramp2, LM_GI_gi042_STsw35, LM_GI_gi042_Spinner_sw25A, LM_GI_gi042_Spinner_sw25B, LM_GI_gi042_Spinner_sw33A, LM_GI_gi042_Spinner_sw33B)
Dim BL_GI_gi043: BL_GI_gi043=Array(LM_GI_gi043_Bumper1_Socket, LM_GI_gi043_Bumper1_ring, LM_GI_gi043_Bumper2_Socket, LM_GI_gi043_Bumper2_ring, LM_GI_gi043_Bumper3_Socket, LM_GI_gi043_Bumper3_ring, LM_GI_gi043_Gate3, LM_GI_gi043_Gate4, LM_GI_gi043_Layer1, LM_GI_gi043_Parts, LM_GI_gi043_PincabRails, LM_GI_gi043_Playfield, LM_GI_gi043_Ramp2, LM_GI_gi043_Spinner_sw17A, LM_GI_gi043_Spinner_sw17B, LM_GI_gi043_Spinner_sw17W, LM_GI_gi043_Spinner_sw17_Wire, LM_GI_gi043_Spinner_sw25A, LM_GI_gi043_Spinner_sw25B, LM_GI_gi043_Spinner_sw33A, LM_GI_gi043_Spinner_sw33B)
Dim BL_L_l1: BL_L_l1=Array(LM_L_l1_LFlipperU, LM_L_l1_Playfield, LM_L_l1_RFlipperU)
Dim BL_L_l10: BL_L_l10=Array(LM_L_l10_Parts, LM_L_l10_Playfield)
Dim BL_L_l11: BL_L_l11=Array(LM_L_l11_Playfield)
Dim BL_L_l12: BL_L_l12=Array(LM_L_l12_Playfield)
Dim BL_L_l14: BL_L_l14=Array(LM_L_l14_Playfield)
Dim BL_L_l15: BL_L_l15=Array(LM_L_l15_Playfield)
Dim BL_L_l17: BL_L_l17=Array(LM_L_l17_LFlipperU, LM_L_l17_Playfield)
Dim BL_L_l18: BL_L_l18=Array(LM_L_l18_Playfield)
Dim BL_L_l19: BL_L_l19=Array(LM_L_l19_Layer1, LM_L_l19_Parts, LM_L_l19_Playfield)
Dim BL_L_l2: BL_L_l2=Array(LM_L_l2_Playfield)
Dim BL_L_l20: BL_L_l20=Array(LM_L_l20_Playfield)
Dim BL_L_l200: BL_L_l200=Array(LM_L_l200_Playfield)
Dim BL_L_l21: BL_L_l21=Array(LM_L_l21_Parts, LM_L_l21_Playfield)
Dim BL_L_l22: BL_L_l22=Array(LM_L_l22_Playfield)
Dim BL_L_l23: BL_L_l23=Array(LM_L_l23_Layer1, LM_L_l23_Parts, LM_L_l23_Playfield)
Dim BL_L_l24: BL_L_l24=Array(LM_L_l24_Playfield)
Dim BL_L_l25: BL_L_l25=Array(LM_L_l25_Parts, LM_L_l25_Playfield)
Dim BL_L_l26: BL_L_l26=Array(LM_L_l26_Bumper1_Socket, LM_L_l26_Bumper1_ring, LM_L_l26_Bumper2_Socket, LM_L_l26_Bumper2_ring, LM_L_l26_Bumper3_Socket, LM_L_l26_Bumper3_ring, LM_L_l26_DTsw1, LM_L_l26_DTsw2, LM_L_l26_DTsw3, LM_L_l26_DTsw4, LM_L_l26_Layer1, LM_L_l26_LeftSling2, LM_L_l26_LeftSling3, LM_L_l26_LeftSling4, LM_L_l26_Parts, LM_L_l26_PincabRails, LM_L_l26_Playfield, LM_L_l26_Ramp1, LM_L_l26_Ramp2, LM_L_l26_RightSling2, LM_L_l26_RightSling3, LM_L_l26_RightSling4, LM_L_l26_STsw27, LM_L_l26_STsw28, LM_L_l26_STsw29, LM_L_l26_STsw30, LM_L_l26_STsw35, LM_L_l26_Spinner_sw17A, LM_L_l26_Spinner_sw17B, LM_L_l26_Spinner_sw17_Wire, LM_L_l26_Spinner_sw25A, LM_L_l26_Spinner_sw25B, LM_L_l26_Spinner_sw25W, LM_L_l26_Spinner_sw25_Wire, LM_L_l26_Spinner_sw33A, LM_L_l26_Spinner_sw33B, LM_L_l26_Spinner_sw33W, LM_L_l26_Spinner_sw33_Wire)
Dim BL_L_l28: BL_L_l28=Array(LM_L_l28_Playfield)
Dim BL_L_l3: BL_L_l3=Array(LM_L_l3_Playfield)
Dim BL_L_l30: BL_L_l30=Array(LM_L_l30_Playfield)
Dim BL_L_l31: BL_L_l31=Array(LM_L_l31_Playfield)
Dim BL_L_l33: BL_L_l33=Array(LM_L_l33_Playfield, LM_L_l33_RFlipperU)
Dim BL_L_l34: BL_L_l34=Array(LM_L_l34_Playfield)
Dim BL_L_l35: BL_L_l35=Array(LM_L_l35_Parts, LM_L_l35_Playfield, LM_L_l35_Spinner_sw25A, LM_L_l35_Spinner_sw25B)
Dim BL_L_l36: BL_L_l36=Array(LM_L_l36_Playfield)
Dim BL_L_l360: BL_L_l360=Array(LM_L_l360_Playfield)
Dim BL_L_l37: BL_L_l37=Array(LM_L_l37_Parts, LM_L_l37_Playfield)
Dim BL_L_l38: BL_L_l38=Array(LM_L_l38_Parts, LM_L_l38_Playfield, LM_L_l38_Spinner_sw17A, LM_L_l38_Spinner_sw17B)
Dim BL_L_l39: BL_L_l39=Array(LM_L_l39_Layer1, LM_L_l39_Parts, LM_L_l39_Playfield)
Dim BL_L_l4: BL_L_l4=Array(LM_L_l4_Playfield)
Dim BL_L_l40: BL_L_l40=Array(LM_L_l40_LeftSling1, LM_L_l40_Parts, LM_L_l40_Playfield, LM_L_l40_Ramp2)
Dim BL_L_l41: BL_L_l41=Array(LM_L_l41_Parts, LM_L_l41_Playfield)
Dim BL_L_l42: BL_L_l42=Array(LM_L_l42_Bumper1_Socket, LM_L_l42_Bumper1_ring, LM_L_l42_Bumper2_Socket, LM_L_l42_Bumper2_ring, LM_L_l42_Bumper3_Socket, LM_L_l42_Bumper3_ring, LM_L_l42_Gate3, LM_L_l42_Gate4, LM_L_l42_Layer1, LM_L_l42_Parts, LM_L_l42_PincabRails, LM_L_l42_Playfield, LM_L_l42_Ramp1, LM_L_l42_Ramp2, LM_L_l42_STsw28, LM_L_l42_STsw29, LM_L_l42_STsw30, LM_L_l42_Spinner_sw17A, LM_L_l42_Spinner_sw17B, LM_L_l42_Spinner_sw17W, LM_L_l42_Spinner_sw17_Wire, LM_L_l42_Spinner_sw25A, LM_L_l42_Spinner_sw25B, LM_L_l42_Spinner_sw25_Wire, LM_L_l42_Spinner_sw33A, LM_L_l42_Spinner_sw33B, LM_L_l42_Spinner_sw33W, LM_L_l42_Spinner_sw33_Wire)
Dim BL_L_l42b: BL_L_l42b=Array(LM_L_l42b_Bumper1_Socket, LM_L_l42b_Bumper1_ring, LM_L_l42b_Bumper2_Socket, LM_L_l42b_Bumper2_ring, LM_L_l42b_Bumper3_Socket, LM_L_l42b_Bumper3_ring, LM_L_l42b_DTsw1, LM_L_l42b_DTsw2, LM_L_l42b_DTsw3, LM_L_l42b_DTsw4, LM_L_l42b_Layer1, LM_L_l42b_Parts, LM_L_l42b_PincabRails, LM_L_l42b_Playfield, LM_L_l42b_Ramp1, LM_L_l42b_Ramp2, LM_L_l42b_Spinner_sw17A, LM_L_l42b_Spinner_sw17B, LM_L_l42b_Spinner_sw25A, LM_L_l42b_Spinner_sw25B, LM_L_l42b_Spinner_sw25W, LM_L_l42b_Spinner_sw25_Wire, LM_L_l42b_Spinner_sw33A, LM_L_l42b_Spinner_sw33B, LM_L_l42b_Spinner_sw33_Wire)
Dim BL_L_l43: BL_L_l43=Array(LM_L_l43_Playfield)
Dim BL_L_l44: BL_L_l44=Array(LM_L_l44_Playfield)
Dim BL_L_l46: BL_L_l46=Array(LM_L_l46_Playfield)
Dim BL_L_l47: BL_L_l47=Array(LM_L_l47_Playfield)
Dim BL_L_l49: BL_L_l49=Array(LM_L_l49_Playfield)
Dim BL_L_l5: BL_L_l5=Array(LM_L_l5_Parts, LM_L_l5_Playfield, LM_L_l5_Spinner_sw33A, LM_L_l5_Spinner_sw33B)
Dim BL_L_l50: BL_L_l50=Array(LM_L_l50_Playfield)
Dim BL_L_l500: BL_L_l500=Array(LM_L_l500_Parts, LM_L_l500_Playfield, LM_L_l500_Spinner_sw25A, LM_L_l500_Spinner_sw25B)
Dim BL_L_l51: BL_L_l51=Array(LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_Spinner_sw33A, LM_L_l51_Spinner_sw33B)
Dim BL_L_l52: BL_L_l52=Array(LM_L_l52_Playfield)
Dim BL_L_l520: BL_L_l520=Array(LM_L_l520_Playfield)
Dim BL_L_l53: BL_L_l53=Array(LM_L_l53_Layer1, LM_L_l53_Parts, LM_L_l53_Playfield)
Dim BL_L_l54: BL_L_l54=Array(LM_L_l54_Parts, LM_L_l54_Playfield, LM_L_l54_Spinner_sw17A, LM_L_l54_Spinner_sw17B, LM_L_l54_Spinner_sw17W, LM_L_l54_Spinner_sw17_Wire)
Dim BL_L_l55: BL_L_l55=Array(LM_L_l55_Parts, LM_L_l55_Playfield)
Dim BL_L_l56: BL_L_l56=Array(LM_L_l56_Playfield)
Dim BL_L_l57: BL_L_l57=Array(LM_L_l57_Parts, LM_L_l57_Playfield)
Dim BL_L_l58: BL_L_l58=Array(LM_L_l58_Parts)
Dim BL_L_l59: BL_L_l59=Array(LM_L_l59_Parts, LM_L_l59_apron1, LM_L_l59_hg_apron2_003)
Dim BL_L_l6: BL_L_l6=Array(LM_L_l6_Parts, LM_L_l6_Playfield)
Dim BL_L_l60: BL_L_l60=Array(LM_L_l60_Playfield)
Dim BL_L_l62: BL_L_l62=Array(LM_L_l62_Playfield)
Dim BL_L_l63: BL_L_l63=Array(LM_L_l63_Playfield)
Dim BL_L_l7: BL_L_l7=Array(LM_L_l7_Parts, LM_L_l7_Playfield)
Dim BL_L_l8: BL_L_l8=Array(LM_L_l8_Parts, LM_L_l8_Playfield)
Dim BL_L_l9: BL_L_l9=Array(LM_L_l9_Parts, LM_L_l9_Playfield)
Dim BL_World: BL_World=Array(BM_Bumper1_Socket, BM_Bumper1_ring, BM_Bumper2_Socket, BM_Bumper2_ring, BM_Bumper3_Socket, BM_Bumper3_ring, BM_DTsw1, BM_DTsw2, BM_DTsw3, BM_DTsw4, BM_Diverter, BM_Gate2, BM_Gate3, BM_Gate4, BM_InstructionCard2, BM_InsturctionCard1, BM_LFlipper, BM_LFlipper1, BM_LFlipper1U, BM_LFlipperU, BM_Layer1, BM_LeftSling1, BM_LeftSling2, BM_LeftSling3, BM_LeftSling4, BM_Parts, BM_PincabRails, BM_Playfield, BM_RFlipper, BM_RFlipperU, BM_Ramp1, BM_Ramp2, BM_RightSling1, BM_RightSling2, BM_RightSling3, BM_RightSling4, BM_Rollover_sw22, BM_Rollover_sw23, BM_Rollover_sw31, BM_STsw26, BM_STsw27, BM_STsw28, BM_STsw29, BM_STsw30, BM_STsw35, BM_SlingArm1, BM_SlingArm2, BM_Spinner_sw17A, BM_Spinner_sw17B, BM_Spinner_sw17W, BM_Spinner_sw17_Wire, BM_Spinner_sw25A, BM_Spinner_sw25B, BM_Spinner_sw25W, BM_Spinner_sw25_Wire, BM_Spinner_sw33A, BM_Spinner_sw33B, BM_Spinner_sw33W, BM_Spinner_sw33_Wire, BM_apron1, BM_hg_apron2_001, BM_hg_apron2_002, BM_hg_apron2_003, BM_hg_apron2_004)
' Global arrays
Dim BG_Bakemap: BG_Bakemap=Array(BM_Bumper1_Socket, BM_Bumper1_ring, BM_Bumper2_Socket, BM_Bumper2_ring, BM_Bumper3_Socket, BM_Bumper3_ring, BM_DTsw1, BM_DTsw2, BM_DTsw3, BM_DTsw4, BM_Diverter, BM_Gate2, BM_Gate3, BM_Gate4, BM_InstructionCard2, BM_InsturctionCard1, BM_LFlipper, BM_LFlipper1, BM_LFlipper1U, BM_LFlipperU, BM_Layer1, BM_LeftSling1, BM_LeftSling2, BM_LeftSling3, BM_LeftSling4, BM_Parts, BM_PincabRails, BM_Playfield, BM_RFlipper, BM_RFlipperU, BM_Ramp1, BM_Ramp2, BM_RightSling1, BM_RightSling2, BM_RightSling3, BM_RightSling4, BM_Rollover_sw22, BM_Rollover_sw23, BM_Rollover_sw31, BM_STsw26, BM_STsw27, BM_STsw28, BM_STsw29, BM_STsw30, BM_STsw35, BM_SlingArm1, BM_SlingArm2, BM_Spinner_sw17A, BM_Spinner_sw17B, BM_Spinner_sw17W, BM_Spinner_sw17_Wire, BM_Spinner_sw25A, BM_Spinner_sw25B, BM_Spinner_sw25W, BM_Spinner_sw25_Wire, BM_Spinner_sw33A, BM_Spinner_sw33B, BM_Spinner_sw33W, BM_Spinner_sw33_Wire, BM_apron1, BM_hg_apron2_001, BM_hg_apron2_002, BM_hg_apron2_003, BM_hg_apron2_004)
Dim BG_Lightmap: BG_Lightmap=Array(LM_GI_gi002_Bumper1_Socket, LM_GI_gi002_LFlipper, LM_GI_gi002_LFlipper1, LM_GI_gi002_LFlipper1U, LM_GI_gi002_LFlipperU, LM_GI_gi002_Layer1, LM_GI_gi002_LeftSling1, LM_GI_gi002_LeftSling2, LM_GI_gi002_LeftSling3, LM_GI_gi002_LeftSling4, LM_GI_gi002_Parts, LM_GI_gi002_Playfield, LM_GI_gi002_RFlipper, LM_GI_gi002_RFlipperU, LM_GI_gi002_Ramp1, LM_GI_gi002_Ramp2, LM_GI_gi002_RightSling1, LM_GI_gi002_RightSling2, LM_GI_gi002_RightSling3, LM_GI_gi002_RightSling4, LM_GI_gi002_Rollover_sw31, LM_GI_gi002_SlingArm1, LM_GI_gi002_SlingArm2, LM_GI_gi002_apron1, LM_GI_gi002_hg_apron2_003, LM_GI_gi003_Bumper2_Socket, LM_GI_gi003_Diverter, LM_GI_gi003_LFlipper, LM_GI_gi003_LFlipper1, LM_GI_gi003_LFlipper1U, LM_GI_gi003_LFlipperU, LM_GI_gi003_Layer1, LM_GI_gi003_LeftSling1, LM_GI_gi003_LeftSling2, LM_GI_gi003_LeftSling3, LM_GI_gi003_LeftSling4, LM_GI_gi003_Parts, LM_GI_gi003_Playfield, LM_GI_gi003_RFlipper, LM_GI_gi003_RFlipperU, LM_GI_gi003_Ramp1, LM_GI_gi003_Ramp2, LM_GI_gi003_RightSling1, _
	LM_GI_gi003_RightSling2, LM_GI_gi003_RightSling3, LM_GI_gi003_RightSling4, LM_GI_gi003_Rollover_sw22, LM_GI_gi003_SlingArm1, LM_GI_gi003_apron1, LM_GI_gi005_Diverter, LM_GI_gi005_Layer1, LM_GI_gi005_LeftSling3, LM_GI_gi005_LeftSling4, LM_GI_gi005_Parts, LM_GI_gi005_Playfield, LM_GI_gi005_Ramp1, LM_GI_gi005_Ramp2, LM_GI_gi005_RightSling1, LM_GI_gi005_RightSling2, LM_GI_gi005_RightSling3, LM_GI_gi005_RightSling4, LM_GI_gi005_STsw35, LM_GI_gi005_SlingArm1, LM_GI_gi005_Spinner_sw25A, LM_GI_gi005_Spinner_sw25B, LM_GI_gi005_Spinner_sw25_Wire, LM_GI_gi008_Bumper2_Socket, LM_GI_gi008_Bumper3_Socket, LM_GI_gi008_Layer1, LM_GI_gi008_LeftSling1, LM_GI_gi008_LeftSling2, LM_GI_gi008_LeftSling3, LM_GI_gi008_LeftSling4, LM_GI_gi008_Parts, LM_GI_gi008_Playfield, LM_GI_gi008_Ramp2, LM_GI_gi008_STsw26, LM_GI_gi008_STsw27, LM_GI_gi008_STsw28, LM_GI_gi008_STsw29, LM_GI_gi008_STsw30, LM_GI_gi008_Spinner_sw17A, LM_GI_gi008_Spinner_sw17B, LM_GI_gi008_Spinner_sw33A, LM_GI_gi008_Spinner_sw33B, LM_GI_gi010_Bumper2_Socket, _
	LM_GI_gi010_Bumper3_Socket, LM_GI_gi010_Layer1, LM_GI_gi010_Parts, LM_GI_gi010_Playfield, LM_GI_gi010_Ramp2, LM_GI_gi010_STsw26, LM_GI_gi010_STsw27, LM_GI_gi010_STsw28, LM_GI_gi010_STsw29, LM_GI_gi010_STsw30, LM_GI_gi010_Spinner_sw17A, LM_GI_gi010_Spinner_sw17B, LM_GI_gi012_Bumper1_Socket, LM_GI_gi012_Bumper1_ring, LM_GI_gi012_Bumper2_Socket, LM_GI_gi012_Bumper2_ring, LM_GI_gi012_Layer1, LM_GI_gi012_Parts, LM_GI_gi012_Playfield, LM_GI_gi012_Ramp2, LM_GI_gi012_STsw27, LM_GI_gi012_STsw28, LM_GI_gi012_STsw29, LM_GI_gi012_STsw30, LM_GI_gi012_Spinner_sw17A, LM_GI_gi012_Spinner_sw17B, LM_GI_gi012_Spinner_sw17W, LM_GI_gi012_Spinner_sw17_Wire, LM_GI_gi014_Bumper1_Socket, LM_GI_gi014_Bumper1_ring, LM_GI_gi014_Bumper2_Socket, LM_GI_gi014_Bumper2_ring, LM_GI_gi014_Bumper3_Socket, LM_GI_gi014_Layer1, LM_GI_gi014_Parts, LM_GI_gi014_Playfield, LM_GI_gi014_Ramp1, LM_GI_gi014_Ramp2, LM_GI_gi014_Spinner_sw17A, LM_GI_gi014_Spinner_sw17B, LM_GI_gi014_Spinner_sw17W, LM_GI_gi014_Spinner_sw17_Wire, LM_GI_gi014_Spinner_sw25A, _
	LM_GI_gi014_Spinner_sw25B, LM_GI_gi014_Spinner_sw25_Wire, LM_GI_gi014_Spinner_sw33A, LM_GI_gi014_Spinner_sw33B, LM_GI_gi014_Spinner_sw33_Wire, LM_GI_gi015_Bumper1_Socket, LM_GI_gi015_Bumper1_ring, LM_GI_gi015_Bumper2_Socket, LM_GI_gi015_Bumper2_ring, LM_GI_gi015_Bumper3_Socket, LM_GI_gi015_DTsw3, LM_GI_gi015_DTsw4, LM_GI_gi015_Layer1, LM_GI_gi015_Parts, LM_GI_gi015_Playfield, LM_GI_gi015_Ramp1, LM_GI_gi015_Ramp2, LM_GI_gi015_RightSling1, LM_GI_gi015_STsw35, LM_GI_gi015_Spinner_sw17A, LM_GI_gi015_Spinner_sw17B, LM_GI_gi015_Spinner_sw25A, LM_GI_gi015_Spinner_sw25B, LM_GI_gi015_Spinner_sw25_Wire, LM_GI_gi015_Spinner_sw33A, LM_GI_gi015_Spinner_sw33B, LM_GI_gi015_Spinner_sw33_Wire, LM_GI_gi017_Bumper2_Socket, LM_GI_gi017_Bumper2_ring, LM_GI_gi017_Bumper3_Socket, LM_GI_gi017_DTsw1, LM_GI_gi017_DTsw2, LM_GI_gi017_DTsw3, LM_GI_gi017_DTsw4, LM_GI_gi017_Layer1, LM_GI_gi017_Parts, LM_GI_gi017_PincabRails, LM_GI_gi017_Playfield, LM_GI_gi017_Ramp1, LM_GI_gi017_Ramp2, LM_GI_gi017_STsw35, LM_GI_gi019_Bumper1_Socket, _
	LM_GI_gi019_Bumper1_ring, LM_GI_gi019_Bumper2_Socket, LM_GI_gi019_Bumper2_ring, LM_GI_gi019_Bumper3_Socket, LM_GI_gi019_Bumper3_ring, LM_GI_gi019_DTsw1, LM_GI_gi019_DTsw2, LM_GI_gi019_DTsw3, LM_GI_gi019_DTsw4, LM_GI_gi019_Layer1, LM_GI_gi019_Parts, LM_GI_gi019_Playfield, LM_GI_gi019_Ramp1, LM_GI_gi019_Ramp2, LM_GI_gi019_STsw35, LM_GI_gi019_Spinner_sw25A, LM_GI_gi019_Spinner_sw25B, LM_GI_gi019_Spinner_sw25_Wire, LM_GI_gi019_Spinner_sw33A, LM_GI_gi019_Spinner_sw33B, LM_GI_gi019_Spinner_sw33_Wire, LM_GI_gi021_Bumper1_Socket, LM_GI_gi021_Bumper1_ring, LM_GI_gi021_Bumper2_Socket, LM_GI_gi021_Bumper2_ring, LM_GI_gi021_Bumper3_Socket, LM_GI_gi021_Bumper3_ring, LM_GI_gi021_DTsw1, LM_GI_gi021_DTsw2, LM_GI_gi021_Gate2, LM_GI_gi021_Layer1, LM_GI_gi021_Parts, LM_GI_gi021_Playfield, LM_GI_gi021_Ramp1, LM_GI_gi021_Ramp2, LM_GI_gi021_Spinner_sw25A, LM_GI_gi021_Spinner_sw25B, LM_GI_gi021_Spinner_sw25_Wire, LM_GI_gi021_Spinner_sw33A, LM_GI_gi021_Spinner_sw33B, LM_GI_gi021_Spinner_sw33_Wire, LM_GI_gi023_Bumper1_Socket, _
	LM_GI_gi023_Bumper1_ring, LM_GI_gi023_Bumper3_Socket, LM_GI_gi023_Bumper3_ring, LM_GI_gi023_DTsw1, LM_GI_gi023_DTsw2, LM_GI_gi023_DTsw3, LM_GI_gi023_DTsw4, LM_GI_gi023_Gate2, LM_GI_gi023_Layer1, LM_GI_gi023_Parts, LM_GI_gi023_Playfield, LM_GI_gi023_Ramp1, LM_GI_gi023_STsw35, LM_GI_gi023_Spinner_sw25A, LM_GI_gi023_Spinner_sw25B, LM_GI_gi023_Spinner_sw33A, LM_GI_gi023_Spinner_sw33B, LM_GI_gi025_Bumper1_Socket, LM_GI_gi025_Bumper1_ring, LM_GI_gi025_Bumper2_Socket, LM_GI_gi025_Bumper2_ring, LM_GI_gi025_Bumper3_Socket, LM_GI_gi025_Layer1, LM_GI_gi025_Parts, LM_GI_gi025_PincabRails, LM_GI_gi025_Playfield, LM_GI_gi025_Ramp1, LM_GI_gi025_Ramp2, LM_GI_gi025_STsw29, LM_GI_gi025_STsw30, LM_GI_gi025_Spinner_sw17A, LM_GI_gi025_Spinner_sw17B, LM_GI_gi025_Spinner_sw17W, LM_GI_gi025_Spinner_sw17_Wire, LM_GI_gi025_Spinner_sw25A, LM_GI_gi025_Spinner_sw25B, LM_GI_gi025_Spinner_sw25_Wire, LM_GI_gi025_Spinner_sw33A, LM_GI_gi025_Spinner_sw33B, LM_GI_gi025_Spinner_sw33_Wire, LM_GI_gi029_Bumper1_Socket, LM_GI_gi029_Bumper1_ring, _
	LM_GI_gi029_Bumper2_Socket, LM_GI_gi029_Bumper2_ring, LM_GI_gi029_Bumper3_Socket, LM_GI_gi029_Bumper3_ring, LM_GI_gi029_Gate3, LM_GI_gi029_Gate4, LM_GI_gi029_Layer1, LM_GI_gi029_Parts, LM_GI_gi029_Playfield, LM_GI_gi029_Ramp2, LM_GI_gi029_Spinner_sw17A, LM_GI_gi029_Spinner_sw17B, LM_GI_gi029_Spinner_sw25A, LM_GI_gi029_Spinner_sw25B, LM_GI_gi029_Spinner_sw33A, LM_GI_gi029_Spinner_sw33B, LM_GI_gi031_Bumper1_Socket, LM_GI_gi031_Gate4, LM_GI_gi031_Layer1, LM_GI_gi031_Parts, LM_GI_gi031_Playfield, LM_GI_gi031_Ramp2, LM_GI_gi031_Spinner_sw17A, LM_GI_gi031_Spinner_sw17B, LM_GI_gi033_Bumper1_Socket, LM_GI_gi033_Bumper2_Socket, LM_GI_gi033_Bumper3_Socket, LM_GI_gi033_Bumper3_ring, LM_GI_gi033_Gate4, LM_GI_gi033_Layer1, LM_GI_gi033_Parts, LM_GI_gi033_Playfield, LM_GI_gi033_Ramp2, LM_GI_gi035_Bumper1_Socket, LM_GI_gi035_Bumper3_Socket, LM_GI_gi035_DTsw1, LM_GI_gi035_DTsw2, LM_GI_gi035_DTsw3, LM_GI_gi035_DTsw4, LM_GI_gi035_Gate3, LM_GI_gi035_Layer1, LM_GI_gi035_Parts, LM_GI_gi035_Playfield, LM_GI_gi035_Ramp1, _
	LM_GI_gi037_Bumper2_Socket, LM_GI_gi037_Bumper3_Socket, LM_GI_gi037_Bumper3_ring, LM_GI_gi037_DTsw1, LM_GI_gi037_DTsw2, LM_GI_gi037_DTsw3, LM_GI_gi037_DTsw4, LM_GI_gi037_Gate3, LM_GI_gi037_Layer1, LM_GI_gi037_Parts, LM_GI_gi037_Playfield, LM_GI_gi037_Ramp1, LM_GI_gi037_Spinner_sw25A, LM_GI_gi037_Spinner_sw25B, LM_GI_gi039_Bumper2_Socket, LM_GI_gi039_Bumper2_ring, LM_GI_gi039_Bumper3_Socket, LM_GI_gi039_Bumper3_ring, LM_GI_gi039_Diverter, LM_GI_gi039_Layer1, LM_GI_gi039_LeftSling1, LM_GI_gi039_LeftSling2, LM_GI_gi039_LeftSling3, LM_GI_gi039_LeftSling4, LM_GI_gi039_Parts, LM_GI_gi039_Playfield, LM_GI_gi039_RFlipper, LM_GI_gi039_RFlipperU, LM_GI_gi039_Ramp1, LM_GI_gi039_Ramp2, LM_GI_gi039_RightSling1, LM_GI_gi039_RightSling2, LM_GI_gi039_RightSling3, LM_GI_gi039_RightSling4, LM_GI_gi039_Rollover_sw22, LM_GI_gi039_Rollover_sw23, LM_GI_gi039_SlingArm1, LM_GI_gi039_SlingArm2, LM_GI_gi039_apron1, LM_GI_gi042_Bumper1_Socket, LM_GI_gi042_Bumper1_ring, LM_GI_gi042_Bumper2_Socket, LM_GI_gi042_Bumper2_ring, _
	LM_GI_gi042_Bumper3_Socket, LM_GI_gi042_DTsw2, LM_GI_gi042_DTsw3, LM_GI_gi042_DTsw4, LM_GI_gi042_Layer1, LM_GI_gi042_Parts, LM_GI_gi042_Playfield, LM_GI_gi042_Ramp1, LM_GI_gi042_Ramp2, LM_GI_gi042_STsw35, LM_GI_gi042_Spinner_sw25A, LM_GI_gi042_Spinner_sw25B, LM_GI_gi042_Spinner_sw33A, LM_GI_gi042_Spinner_sw33B, LM_GI_gi043_Bumper1_Socket, LM_GI_gi043_Bumper1_ring, LM_GI_gi043_Bumper2_Socket, LM_GI_gi043_Bumper2_ring, LM_GI_gi043_Bumper3_Socket, LM_GI_gi043_Bumper3_ring, LM_GI_gi043_Gate3, LM_GI_gi043_Gate4, LM_GI_gi043_Layer1, LM_GI_gi043_Parts, LM_GI_gi043_PincabRails, LM_GI_gi043_Playfield, LM_GI_gi043_Ramp2, LM_GI_gi043_Spinner_sw17A, LM_GI_gi043_Spinner_sw17B, LM_GI_gi043_Spinner_sw17W, LM_GI_gi043_Spinner_sw17_Wire, LM_GI_gi043_Spinner_sw25A, LM_GI_gi043_Spinner_sw25B, LM_GI_gi043_Spinner_sw33A, LM_GI_gi043_Spinner_sw33B, LM_L_l1_LFlipperU, LM_L_l1_Playfield, LM_L_l1_RFlipperU, LM_L_l10_Parts, LM_L_l10_Playfield, LM_L_l11_Playfield, LM_L_l12_Playfield, LM_L_l14_Playfield, LM_L_l15_Playfield, _
	LM_L_l17_LFlipperU, LM_L_l17_Playfield, LM_L_l18_Playfield, LM_L_l19_Layer1, LM_L_l19_Parts, LM_L_l19_Playfield, LM_L_l2_Playfield, LM_L_l20_Playfield, LM_L_l200_Playfield, LM_L_l21_Parts, LM_L_l21_Playfield, LM_L_l22_Playfield, LM_L_l23_Layer1, LM_L_l23_Parts, LM_L_l23_Playfield, LM_L_l24_Playfield, LM_L_l25_Parts, LM_L_l25_Playfield, LM_L_l26_Bumper1_Socket, LM_L_l26_Bumper1_ring, LM_L_l26_Bumper2_Socket, LM_L_l26_Bumper2_ring, LM_L_l26_Bumper3_Socket, LM_L_l26_Bumper3_ring, LM_L_l26_DTsw1, LM_L_l26_DTsw2, LM_L_l26_DTsw3, LM_L_l26_DTsw4, LM_L_l26_Layer1, LM_L_l26_LeftSling2, LM_L_l26_LeftSling3, LM_L_l26_LeftSling4, LM_L_l26_Parts, LM_L_l26_PincabRails, LM_L_l26_Playfield, LM_L_l26_Ramp1, LM_L_l26_Ramp2, LM_L_l26_RightSling2, LM_L_l26_RightSling3, LM_L_l26_RightSling4, LM_L_l26_STsw27, LM_L_l26_STsw28, LM_L_l26_STsw29, LM_L_l26_STsw30, LM_L_l26_STsw35, LM_L_l26_Spinner_sw17A, LM_L_l26_Spinner_sw17B, LM_L_l26_Spinner_sw17_Wire, LM_L_l26_Spinner_sw25A, LM_L_l26_Spinner_sw25B, LM_L_l26_Spinner_sw25W, _
	LM_L_l26_Spinner_sw25_Wire, LM_L_l26_Spinner_sw33A, LM_L_l26_Spinner_sw33B, LM_L_l26_Spinner_sw33W, LM_L_l26_Spinner_sw33_Wire, LM_L_l28_Playfield, LM_L_l3_Playfield, LM_L_l30_Playfield, LM_L_l31_Playfield, LM_L_l33_Playfield, LM_L_l33_RFlipperU, LM_L_l34_Playfield, LM_L_l35_Parts, LM_L_l35_Playfield, LM_L_l35_Spinner_sw25A, LM_L_l35_Spinner_sw25B, LM_L_l36_Playfield, LM_L_l360_Playfield, LM_L_l37_Parts, LM_L_l37_Playfield, LM_L_l38_Parts, LM_L_l38_Playfield, LM_L_l38_Spinner_sw17A, LM_L_l38_Spinner_sw17B, LM_L_l39_Layer1, LM_L_l39_Parts, LM_L_l39_Playfield, LM_L_l4_Playfield, LM_L_l40_LeftSling1, LM_L_l40_Parts, LM_L_l40_Playfield, LM_L_l40_Ramp2, LM_L_l41_Parts, LM_L_l41_Playfield, LM_L_l42_Bumper1_Socket, LM_L_l42_Bumper1_ring, LM_L_l42_Bumper2_Socket, LM_L_l42_Bumper2_ring, LM_L_l42_Bumper3_Socket, LM_L_l42_Bumper3_ring, LM_L_l42_Gate3, LM_L_l42_Gate4, LM_L_l42_Layer1, LM_L_l42_Parts, LM_L_l42_PincabRails, LM_L_l42_Playfield, LM_L_l42_Ramp1, LM_L_l42_Ramp2, LM_L_l42_STsw28, LM_L_l42_STsw29, _
	LM_L_l42_STsw30, LM_L_l42_Spinner_sw17A, LM_L_l42_Spinner_sw17B, LM_L_l42_Spinner_sw17W, LM_L_l42_Spinner_sw17_Wire, LM_L_l42_Spinner_sw25A, LM_L_l42_Spinner_sw25B, LM_L_l42_Spinner_sw25_Wire, LM_L_l42_Spinner_sw33A, LM_L_l42_Spinner_sw33B, LM_L_l42_Spinner_sw33W, LM_L_l42_Spinner_sw33_Wire, LM_L_l42b_Bumper1_Socket, LM_L_l42b_Bumper1_ring, LM_L_l42b_Bumper2_Socket, LM_L_l42b_Bumper2_ring, LM_L_l42b_Bumper3_Socket, LM_L_l42b_Bumper3_ring, LM_L_l42b_DTsw1, LM_L_l42b_DTsw2, LM_L_l42b_DTsw3, LM_L_l42b_DTsw4, LM_L_l42b_Layer1, LM_L_l42b_Parts, LM_L_l42b_PincabRails, LM_L_l42b_Playfield, LM_L_l42b_Ramp1, LM_L_l42b_Ramp2, LM_L_l42b_Spinner_sw17A, LM_L_l42b_Spinner_sw17B, LM_L_l42b_Spinner_sw25A, LM_L_l42b_Spinner_sw25B, LM_L_l42b_Spinner_sw25W, LM_L_l42b_Spinner_sw25_Wire, LM_L_l42b_Spinner_sw33A, LM_L_l42b_Spinner_sw33B, LM_L_l42b_Spinner_sw33_Wire, LM_L_l43_Playfield, LM_L_l44_Playfield, LM_L_l46_Playfield, LM_L_l47_Playfield, LM_L_l49_Playfield, LM_L_l5_Parts, LM_L_l5_Playfield, LM_L_l5_Spinner_sw33A, _
	LM_L_l5_Spinner_sw33B, LM_L_l50_Playfield, LM_L_l500_Parts, LM_L_l500_Playfield, LM_L_l500_Spinner_sw25A, LM_L_l500_Spinner_sw25B, LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_Spinner_sw33A, LM_L_l51_Spinner_sw33B, LM_L_l52_Playfield, LM_L_l520_Playfield, LM_L_l53_Layer1, LM_L_l53_Parts, LM_L_l53_Playfield, LM_L_l54_Parts, LM_L_l54_Playfield, LM_L_l54_Spinner_sw17A, LM_L_l54_Spinner_sw17B, LM_L_l54_Spinner_sw17W, LM_L_l54_Spinner_sw17_Wire, LM_L_l55_Parts, LM_L_l55_Playfield, LM_L_l56_Playfield, LM_L_l57_Parts, LM_L_l57_Playfield, LM_L_l58_Parts, LM_L_l59_Parts, LM_L_l59_apron1, LM_L_l59_hg_apron2_003, LM_L_l6_Parts, LM_L_l6_Playfield, LM_L_l60_Playfield, LM_L_l62_Playfield, LM_L_l63_Playfield, LM_L_l7_Parts, LM_L_l7_Playfield, LM_L_l8_Parts, LM_L_l8_Playfield, LM_L_l9_Parts, LM_L_l9_Playfield)
Dim BG_All: BG_All=Array(BM_Bumper1_Socket, BM_Bumper1_ring, BM_Bumper2_Socket, BM_Bumper2_ring, BM_Bumper3_Socket, BM_Bumper3_ring, BM_DTsw1, BM_DTsw2, BM_DTsw3, BM_DTsw4, BM_Diverter, BM_Gate2, BM_Gate3, BM_Gate4, BM_InstructionCard2, BM_InsturctionCard1, BM_LFlipper, BM_LFlipper1, BM_LFlipper1U, BM_LFlipperU, BM_Layer1, BM_LeftSling1, BM_LeftSling2, BM_LeftSling3, BM_LeftSling4, BM_Parts, BM_PincabRails, BM_Playfield, BM_RFlipper, BM_RFlipperU, BM_Ramp1, BM_Ramp2, BM_RightSling1, BM_RightSling2, BM_RightSling3, BM_RightSling4, BM_Rollover_sw22, BM_Rollover_sw23, BM_Rollover_sw31, BM_STsw26, BM_STsw27, BM_STsw28, BM_STsw29, BM_STsw30, BM_STsw35, BM_SlingArm1, BM_SlingArm2, BM_Spinner_sw17A, BM_Spinner_sw17B, BM_Spinner_sw17W, BM_Spinner_sw17_Wire, BM_Spinner_sw25A, BM_Spinner_sw25B, BM_Spinner_sw25W, BM_Spinner_sw25_Wire, BM_Spinner_sw33A, BM_Spinner_sw33B, BM_Spinner_sw33W, BM_Spinner_sw33_Wire, BM_apron1, BM_hg_apron2_001, BM_hg_apron2_002, BM_hg_apron2_003, BM_hg_apron2_004, LM_GI_gi002_Bumper1_Socket, _
	LM_GI_gi002_LFlipper, LM_GI_gi002_LFlipper1, LM_GI_gi002_LFlipper1U, LM_GI_gi002_LFlipperU, LM_GI_gi002_Layer1, LM_GI_gi002_LeftSling1, LM_GI_gi002_LeftSling2, LM_GI_gi002_LeftSling3, LM_GI_gi002_LeftSling4, LM_GI_gi002_Parts, LM_GI_gi002_Playfield, LM_GI_gi002_RFlipper, LM_GI_gi002_RFlipperU, LM_GI_gi002_Ramp1, LM_GI_gi002_Ramp2, LM_GI_gi002_RightSling1, LM_GI_gi002_RightSling2, LM_GI_gi002_RightSling3, LM_GI_gi002_RightSling4, LM_GI_gi002_Rollover_sw31, LM_GI_gi002_SlingArm1, LM_GI_gi002_SlingArm2, LM_GI_gi002_apron1, LM_GI_gi002_hg_apron2_003, LM_GI_gi003_Bumper2_Socket, LM_GI_gi003_Diverter, LM_GI_gi003_LFlipper, LM_GI_gi003_LFlipper1, LM_GI_gi003_LFlipper1U, LM_GI_gi003_LFlipperU, LM_GI_gi003_Layer1, LM_GI_gi003_LeftSling1, LM_GI_gi003_LeftSling2, LM_GI_gi003_LeftSling3, LM_GI_gi003_LeftSling4, LM_GI_gi003_Parts, LM_GI_gi003_Playfield, LM_GI_gi003_RFlipper, LM_GI_gi003_RFlipperU, LM_GI_gi003_Ramp1, LM_GI_gi003_Ramp2, LM_GI_gi003_RightSling1, LM_GI_gi003_RightSling2, LM_GI_gi003_RightSling3, _
	LM_GI_gi003_RightSling4, LM_GI_gi003_Rollover_sw22, LM_GI_gi003_SlingArm1, LM_GI_gi003_apron1, LM_GI_gi005_Diverter, LM_GI_gi005_Layer1, LM_GI_gi005_LeftSling3, LM_GI_gi005_LeftSling4, LM_GI_gi005_Parts, LM_GI_gi005_Playfield, LM_GI_gi005_Ramp1, LM_GI_gi005_Ramp2, LM_GI_gi005_RightSling1, LM_GI_gi005_RightSling2, LM_GI_gi005_RightSling3, LM_GI_gi005_RightSling4, LM_GI_gi005_STsw35, LM_GI_gi005_SlingArm1, LM_GI_gi005_Spinner_sw25A, LM_GI_gi005_Spinner_sw25B, LM_GI_gi005_Spinner_sw25_Wire, LM_GI_gi008_Bumper2_Socket, LM_GI_gi008_Bumper3_Socket, LM_GI_gi008_Layer1, LM_GI_gi008_LeftSling1, LM_GI_gi008_LeftSling2, LM_GI_gi008_LeftSling3, LM_GI_gi008_LeftSling4, LM_GI_gi008_Parts, LM_GI_gi008_Playfield, LM_GI_gi008_Ramp2, LM_GI_gi008_STsw26, LM_GI_gi008_STsw27, LM_GI_gi008_STsw28, LM_GI_gi008_STsw29, LM_GI_gi008_STsw30, LM_GI_gi008_Spinner_sw17A, LM_GI_gi008_Spinner_sw17B, LM_GI_gi008_Spinner_sw33A, LM_GI_gi008_Spinner_sw33B, LM_GI_gi010_Bumper2_Socket, LM_GI_gi010_Bumper3_Socket, LM_GI_gi010_Layer1, _
	LM_GI_gi010_Parts, LM_GI_gi010_Playfield, LM_GI_gi010_Ramp2, LM_GI_gi010_STsw26, LM_GI_gi010_STsw27, LM_GI_gi010_STsw28, LM_GI_gi010_STsw29, LM_GI_gi010_STsw30, LM_GI_gi010_Spinner_sw17A, LM_GI_gi010_Spinner_sw17B, LM_GI_gi012_Bumper1_Socket, LM_GI_gi012_Bumper1_ring, LM_GI_gi012_Bumper2_Socket, LM_GI_gi012_Bumper2_ring, LM_GI_gi012_Layer1, LM_GI_gi012_Parts, LM_GI_gi012_Playfield, LM_GI_gi012_Ramp2, LM_GI_gi012_STsw27, LM_GI_gi012_STsw28, LM_GI_gi012_STsw29, LM_GI_gi012_STsw30, LM_GI_gi012_Spinner_sw17A, LM_GI_gi012_Spinner_sw17B, LM_GI_gi012_Spinner_sw17W, LM_GI_gi012_Spinner_sw17_Wire, LM_GI_gi014_Bumper1_Socket, LM_GI_gi014_Bumper1_ring, LM_GI_gi014_Bumper2_Socket, LM_GI_gi014_Bumper2_ring, LM_GI_gi014_Bumper3_Socket, LM_GI_gi014_Layer1, LM_GI_gi014_Parts, LM_GI_gi014_Playfield, LM_GI_gi014_Ramp1, LM_GI_gi014_Ramp2, LM_GI_gi014_Spinner_sw17A, LM_GI_gi014_Spinner_sw17B, LM_GI_gi014_Spinner_sw17W, LM_GI_gi014_Spinner_sw17_Wire, LM_GI_gi014_Spinner_sw25A, LM_GI_gi014_Spinner_sw25B, _
	LM_GI_gi014_Spinner_sw25_Wire, LM_GI_gi014_Spinner_sw33A, LM_GI_gi014_Spinner_sw33B, LM_GI_gi014_Spinner_sw33_Wire, LM_GI_gi015_Bumper1_Socket, LM_GI_gi015_Bumper1_ring, LM_GI_gi015_Bumper2_Socket, LM_GI_gi015_Bumper2_ring, LM_GI_gi015_Bumper3_Socket, LM_GI_gi015_DTsw3, LM_GI_gi015_DTsw4, LM_GI_gi015_Layer1, LM_GI_gi015_Parts, LM_GI_gi015_Playfield, LM_GI_gi015_Ramp1, LM_GI_gi015_Ramp2, LM_GI_gi015_RightSling1, LM_GI_gi015_STsw35, LM_GI_gi015_Spinner_sw17A, LM_GI_gi015_Spinner_sw17B, LM_GI_gi015_Spinner_sw25A, LM_GI_gi015_Spinner_sw25B, LM_GI_gi015_Spinner_sw25_Wire, LM_GI_gi015_Spinner_sw33A, LM_GI_gi015_Spinner_sw33B, LM_GI_gi015_Spinner_sw33_Wire, LM_GI_gi017_Bumper2_Socket, LM_GI_gi017_Bumper2_ring, LM_GI_gi017_Bumper3_Socket, LM_GI_gi017_DTsw1, LM_GI_gi017_DTsw2, LM_GI_gi017_DTsw3, LM_GI_gi017_DTsw4, LM_GI_gi017_Layer1, LM_GI_gi017_Parts, LM_GI_gi017_PincabRails, LM_GI_gi017_Playfield, LM_GI_gi017_Ramp1, LM_GI_gi017_Ramp2, LM_GI_gi017_STsw35, LM_GI_gi019_Bumper1_Socket, LM_GI_gi019_Bumper1_ring, _
	LM_GI_gi019_Bumper2_Socket, LM_GI_gi019_Bumper2_ring, LM_GI_gi019_Bumper3_Socket, LM_GI_gi019_Bumper3_ring, LM_GI_gi019_DTsw1, LM_GI_gi019_DTsw2, LM_GI_gi019_DTsw3, LM_GI_gi019_DTsw4, LM_GI_gi019_Layer1, LM_GI_gi019_Parts, LM_GI_gi019_Playfield, LM_GI_gi019_Ramp1, LM_GI_gi019_Ramp2, LM_GI_gi019_STsw35, LM_GI_gi019_Spinner_sw25A, LM_GI_gi019_Spinner_sw25B, LM_GI_gi019_Spinner_sw25_Wire, LM_GI_gi019_Spinner_sw33A, LM_GI_gi019_Spinner_sw33B, LM_GI_gi019_Spinner_sw33_Wire, LM_GI_gi021_Bumper1_Socket, LM_GI_gi021_Bumper1_ring, LM_GI_gi021_Bumper2_Socket, LM_GI_gi021_Bumper2_ring, LM_GI_gi021_Bumper3_Socket, LM_GI_gi021_Bumper3_ring, LM_GI_gi021_DTsw1, LM_GI_gi021_DTsw2, LM_GI_gi021_Gate2, LM_GI_gi021_Layer1, LM_GI_gi021_Parts, LM_GI_gi021_Playfield, LM_GI_gi021_Ramp1, LM_GI_gi021_Ramp2, LM_GI_gi021_Spinner_sw25A, LM_GI_gi021_Spinner_sw25B, LM_GI_gi021_Spinner_sw25_Wire, LM_GI_gi021_Spinner_sw33A, LM_GI_gi021_Spinner_sw33B, LM_GI_gi021_Spinner_sw33_Wire, LM_GI_gi023_Bumper1_Socket, LM_GI_gi023_Bumper1_ring, _
	LM_GI_gi023_Bumper3_Socket, LM_GI_gi023_Bumper3_ring, LM_GI_gi023_DTsw1, LM_GI_gi023_DTsw2, LM_GI_gi023_DTsw3, LM_GI_gi023_DTsw4, LM_GI_gi023_Gate2, LM_GI_gi023_Layer1, LM_GI_gi023_Parts, LM_GI_gi023_Playfield, LM_GI_gi023_Ramp1, LM_GI_gi023_STsw35, LM_GI_gi023_Spinner_sw25A, LM_GI_gi023_Spinner_sw25B, LM_GI_gi023_Spinner_sw33A, LM_GI_gi023_Spinner_sw33B, LM_GI_gi025_Bumper1_Socket, LM_GI_gi025_Bumper1_ring, LM_GI_gi025_Bumper2_Socket, LM_GI_gi025_Bumper2_ring, LM_GI_gi025_Bumper3_Socket, LM_GI_gi025_Layer1, LM_GI_gi025_Parts, LM_GI_gi025_PincabRails, LM_GI_gi025_Playfield, LM_GI_gi025_Ramp1, LM_GI_gi025_Ramp2, LM_GI_gi025_STsw29, LM_GI_gi025_STsw30, LM_GI_gi025_Spinner_sw17A, LM_GI_gi025_Spinner_sw17B, LM_GI_gi025_Spinner_sw17W, LM_GI_gi025_Spinner_sw17_Wire, LM_GI_gi025_Spinner_sw25A, LM_GI_gi025_Spinner_sw25B, LM_GI_gi025_Spinner_sw25_Wire, LM_GI_gi025_Spinner_sw33A, LM_GI_gi025_Spinner_sw33B, LM_GI_gi025_Spinner_sw33_Wire, LM_GI_gi029_Bumper1_Socket, LM_GI_gi029_Bumper1_ring, LM_GI_gi029_Bumper2_Socket, _
	LM_GI_gi029_Bumper2_ring, LM_GI_gi029_Bumper3_Socket, LM_GI_gi029_Bumper3_ring, LM_GI_gi029_Gate3, LM_GI_gi029_Gate4, LM_GI_gi029_Layer1, LM_GI_gi029_Parts, LM_GI_gi029_Playfield, LM_GI_gi029_Ramp2, LM_GI_gi029_Spinner_sw17A, LM_GI_gi029_Spinner_sw17B, LM_GI_gi029_Spinner_sw25A, LM_GI_gi029_Spinner_sw25B, LM_GI_gi029_Spinner_sw33A, LM_GI_gi029_Spinner_sw33B, LM_GI_gi031_Bumper1_Socket, LM_GI_gi031_Gate4, LM_GI_gi031_Layer1, LM_GI_gi031_Parts, LM_GI_gi031_Playfield, LM_GI_gi031_Ramp2, LM_GI_gi031_Spinner_sw17A, LM_GI_gi031_Spinner_sw17B, LM_GI_gi033_Bumper1_Socket, LM_GI_gi033_Bumper2_Socket, LM_GI_gi033_Bumper3_Socket, LM_GI_gi033_Bumper3_ring, LM_GI_gi033_Gate4, LM_GI_gi033_Layer1, LM_GI_gi033_Parts, LM_GI_gi033_Playfield, LM_GI_gi033_Ramp2, LM_GI_gi035_Bumper1_Socket, LM_GI_gi035_Bumper3_Socket, LM_GI_gi035_DTsw1, LM_GI_gi035_DTsw2, LM_GI_gi035_DTsw3, LM_GI_gi035_DTsw4, LM_GI_gi035_Gate3, LM_GI_gi035_Layer1, LM_GI_gi035_Parts, LM_GI_gi035_Playfield, LM_GI_gi035_Ramp1, LM_GI_gi037_Bumper2_Socket, _
	LM_GI_gi037_Bumper3_Socket, LM_GI_gi037_Bumper3_ring, LM_GI_gi037_DTsw1, LM_GI_gi037_DTsw2, LM_GI_gi037_DTsw3, LM_GI_gi037_DTsw4, LM_GI_gi037_Gate3, LM_GI_gi037_Layer1, LM_GI_gi037_Parts, LM_GI_gi037_Playfield, LM_GI_gi037_Ramp1, LM_GI_gi037_Spinner_sw25A, LM_GI_gi037_Spinner_sw25B, LM_GI_gi039_Bumper2_Socket, LM_GI_gi039_Bumper2_ring, LM_GI_gi039_Bumper3_Socket, LM_GI_gi039_Bumper3_ring, LM_GI_gi039_Diverter, LM_GI_gi039_Layer1, LM_GI_gi039_LeftSling1, LM_GI_gi039_LeftSling2, LM_GI_gi039_LeftSling3, LM_GI_gi039_LeftSling4, LM_GI_gi039_Parts, LM_GI_gi039_Playfield, LM_GI_gi039_RFlipper, LM_GI_gi039_RFlipperU, LM_GI_gi039_Ramp1, LM_GI_gi039_Ramp2, LM_GI_gi039_RightSling1, LM_GI_gi039_RightSling2, LM_GI_gi039_RightSling3, LM_GI_gi039_RightSling4, LM_GI_gi039_Rollover_sw22, LM_GI_gi039_Rollover_sw23, LM_GI_gi039_SlingArm1, LM_GI_gi039_SlingArm2, LM_GI_gi039_apron1, LM_GI_gi042_Bumper1_Socket, LM_GI_gi042_Bumper1_ring, LM_GI_gi042_Bumper2_Socket, LM_GI_gi042_Bumper2_ring, LM_GI_gi042_Bumper3_Socket, _
	LM_GI_gi042_DTsw2, LM_GI_gi042_DTsw3, LM_GI_gi042_DTsw4, LM_GI_gi042_Layer1, LM_GI_gi042_Parts, LM_GI_gi042_Playfield, LM_GI_gi042_Ramp1, LM_GI_gi042_Ramp2, LM_GI_gi042_STsw35, LM_GI_gi042_Spinner_sw25A, LM_GI_gi042_Spinner_sw25B, LM_GI_gi042_Spinner_sw33A, LM_GI_gi042_Spinner_sw33B, LM_GI_gi043_Bumper1_Socket, LM_GI_gi043_Bumper1_ring, LM_GI_gi043_Bumper2_Socket, LM_GI_gi043_Bumper2_ring, LM_GI_gi043_Bumper3_Socket, LM_GI_gi043_Bumper3_ring, LM_GI_gi043_Gate3, LM_GI_gi043_Gate4, LM_GI_gi043_Layer1, LM_GI_gi043_Parts, LM_GI_gi043_PincabRails, LM_GI_gi043_Playfield, LM_GI_gi043_Ramp2, LM_GI_gi043_Spinner_sw17A, LM_GI_gi043_Spinner_sw17B, LM_GI_gi043_Spinner_sw17W, LM_GI_gi043_Spinner_sw17_Wire, LM_GI_gi043_Spinner_sw25A, LM_GI_gi043_Spinner_sw25B, LM_GI_gi043_Spinner_sw33A, LM_GI_gi043_Spinner_sw33B, LM_L_l1_LFlipperU, LM_L_l1_Playfield, LM_L_l1_RFlipperU, LM_L_l10_Parts, LM_L_l10_Playfield, LM_L_l11_Playfield, LM_L_l12_Playfield, LM_L_l14_Playfield, LM_L_l15_Playfield, LM_L_l17_LFlipperU, LM_L_l17_Playfield, _
	LM_L_l18_Playfield, LM_L_l19_Layer1, LM_L_l19_Parts, LM_L_l19_Playfield, LM_L_l2_Playfield, LM_L_l20_Playfield, LM_L_l200_Playfield, LM_L_l21_Parts, LM_L_l21_Playfield, LM_L_l22_Playfield, LM_L_l23_Layer1, LM_L_l23_Parts, LM_L_l23_Playfield, LM_L_l24_Playfield, LM_L_l25_Parts, LM_L_l25_Playfield, LM_L_l26_Bumper1_Socket, LM_L_l26_Bumper1_ring, LM_L_l26_Bumper2_Socket, LM_L_l26_Bumper2_ring, LM_L_l26_Bumper3_Socket, LM_L_l26_Bumper3_ring, LM_L_l26_DTsw1, LM_L_l26_DTsw2, LM_L_l26_DTsw3, LM_L_l26_DTsw4, LM_L_l26_Layer1, LM_L_l26_LeftSling2, LM_L_l26_LeftSling3, LM_L_l26_LeftSling4, LM_L_l26_Parts, LM_L_l26_PincabRails, LM_L_l26_Playfield, LM_L_l26_Ramp1, LM_L_l26_Ramp2, LM_L_l26_RightSling2, LM_L_l26_RightSling3, LM_L_l26_RightSling4, LM_L_l26_STsw27, LM_L_l26_STsw28, LM_L_l26_STsw29, LM_L_l26_STsw30, LM_L_l26_STsw35, LM_L_l26_Spinner_sw17A, LM_L_l26_Spinner_sw17B, LM_L_l26_Spinner_sw17_Wire, LM_L_l26_Spinner_sw25A, LM_L_l26_Spinner_sw25B, LM_L_l26_Spinner_sw25W, LM_L_l26_Spinner_sw25_Wire, _
	LM_L_l26_Spinner_sw33A, LM_L_l26_Spinner_sw33B, LM_L_l26_Spinner_sw33W, LM_L_l26_Spinner_sw33_Wire, LM_L_l28_Playfield, LM_L_l3_Playfield, LM_L_l30_Playfield, LM_L_l31_Playfield, LM_L_l33_Playfield, LM_L_l33_RFlipperU, LM_L_l34_Playfield, LM_L_l35_Parts, LM_L_l35_Playfield, LM_L_l35_Spinner_sw25A, LM_L_l35_Spinner_sw25B, LM_L_l36_Playfield, LM_L_l360_Playfield, LM_L_l37_Parts, LM_L_l37_Playfield, LM_L_l38_Parts, LM_L_l38_Playfield, LM_L_l38_Spinner_sw17A, LM_L_l38_Spinner_sw17B, LM_L_l39_Layer1, LM_L_l39_Parts, LM_L_l39_Playfield, LM_L_l4_Playfield, LM_L_l40_LeftSling1, LM_L_l40_Parts, LM_L_l40_Playfield, LM_L_l40_Ramp2, LM_L_l41_Parts, LM_L_l41_Playfield, LM_L_l42_Bumper1_Socket, LM_L_l42_Bumper1_ring, LM_L_l42_Bumper2_Socket, LM_L_l42_Bumper2_ring, LM_L_l42_Bumper3_Socket, LM_L_l42_Bumper3_ring, LM_L_l42_Gate3, LM_L_l42_Gate4, LM_L_l42_Layer1, LM_L_l42_Parts, LM_L_l42_PincabRails, LM_L_l42_Playfield, LM_L_l42_Ramp1, LM_L_l42_Ramp2, LM_L_l42_STsw28, LM_L_l42_STsw29, LM_L_l42_STsw30, LM_L_l42_Spinner_sw17A, _
	LM_L_l42_Spinner_sw17B, LM_L_l42_Spinner_sw17W, LM_L_l42_Spinner_sw17_Wire, LM_L_l42_Spinner_sw25A, LM_L_l42_Spinner_sw25B, LM_L_l42_Spinner_sw25_Wire, LM_L_l42_Spinner_sw33A, LM_L_l42_Spinner_sw33B, LM_L_l42_Spinner_sw33W, LM_L_l42_Spinner_sw33_Wire, LM_L_l42b_Bumper1_Socket, LM_L_l42b_Bumper1_ring, LM_L_l42b_Bumper2_Socket, LM_L_l42b_Bumper2_ring, LM_L_l42b_Bumper3_Socket, LM_L_l42b_Bumper3_ring, LM_L_l42b_DTsw1, LM_L_l42b_DTsw2, LM_L_l42b_DTsw3, LM_L_l42b_DTsw4, LM_L_l42b_Layer1, LM_L_l42b_Parts, LM_L_l42b_PincabRails, LM_L_l42b_Playfield, LM_L_l42b_Ramp1, LM_L_l42b_Ramp2, LM_L_l42b_Spinner_sw17A, LM_L_l42b_Spinner_sw17B, LM_L_l42b_Spinner_sw25A, LM_L_l42b_Spinner_sw25B, LM_L_l42b_Spinner_sw25W, LM_L_l42b_Spinner_sw25_Wire, LM_L_l42b_Spinner_sw33A, LM_L_l42b_Spinner_sw33B, LM_L_l42b_Spinner_sw33_Wire, LM_L_l43_Playfield, LM_L_l44_Playfield, LM_L_l46_Playfield, LM_L_l47_Playfield, LM_L_l49_Playfield, LM_L_l5_Parts, LM_L_l5_Playfield, LM_L_l5_Spinner_sw33A, LM_L_l5_Spinner_sw33B, LM_L_l50_Playfield, _
	LM_L_l500_Parts, LM_L_l500_Playfield, LM_L_l500_Spinner_sw25A, LM_L_l500_Spinner_sw25B, LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_Spinner_sw33A, LM_L_l51_Spinner_sw33B, LM_L_l52_Playfield, LM_L_l520_Playfield, LM_L_l53_Layer1, LM_L_l53_Parts, LM_L_l53_Playfield, LM_L_l54_Parts, LM_L_l54_Playfield, LM_L_l54_Spinner_sw17A, LM_L_l54_Spinner_sw17B, LM_L_l54_Spinner_sw17W, LM_L_l54_Spinner_sw17_Wire, LM_L_l55_Parts, LM_L_l55_Playfield, LM_L_l56_Playfield, LM_L_l57_Parts, LM_L_l57_Playfield, LM_L_l58_Parts, LM_L_l59_Parts, LM_L_l59_apron1, LM_L_l59_hg_apron2_003, LM_L_l6_Parts, LM_L_l6_Playfield, LM_L_l60_Playfield, LM_L_l62_Playfield, LM_L_l63_Playfield, LM_L_l7_Parts, LM_L_l7_Playfield, LM_L_l8_Parts, LM_L_l8_Playfield, LM_L_l9_Parts, LM_L_l9_Playfield)
' VLM  Arrays - End


'*******************************************
'  ZOPT: User Options
'*******************************************

Dim LightLevel : LightLevel = 0.25				' Level of room lighting (0 to 1), where 0 is dark and 100 is brightest
Dim ColorLUT : ColorLUT = 1						' Color desaturation LUTs: 1 to 11, where 1 is normal and 11 is black'n'white
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1

'----- VR Room Auto-Detect -----
Dim VR_Obj, VRRoom, VRSound, VRSoundVolume, VRSoundInit, CabRails, BPRails, BPRamp1, BPRamp2

Const LiveViewVRSim = 0		' 0 = Default, 1 = View table in VR mode in "Live View Editor"

VRRoom = 3
If RenderingMode = 2 Or LiveViewVRSim = 1 Then
	VRSoundInit = 1
Else
	VRSoundInit = 3
End If
VRSoundVolume = 0.1

' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

	' Color Saturation
    ColorLUT = Table1.Option("Color Saturation", 1, 11, 1, 1, 0, _
		Array("Normal", "Desaturated 10%", "Desaturated 20%", "Desaturated 30%", "Desaturated 40%", "Desaturated 50%", _
        "Desaturated 60%", "Desaturated 70%", "Desaturated 80%", "Desaturated 90%", "Black 'n White"))
	if ColorLUT = 1 Then Table1.ColorGradeImage = ""
	if ColorLUT = 2 Then Table1.ColorGradeImage = "colorgradelut256x16-10"
	if ColorLUT = 3 Then Table1.ColorGradeImage = "colorgradelut256x16-20"
	if ColorLUT = 4 Then Table1.ColorGradeImage = "colorgradelut256x16-30"
	if ColorLUT = 5 Then Table1.ColorGradeImage = "colorgradelut256x16-40"
	if ColorLUT = 6 Then Table1.ColorGradeImage = "colorgradelut256x16-50"
	if ColorLUT = 7 Then Table1.ColorGradeImage = "colorgradelut256x16-60"
	if ColorLUT = 8 Then Table1.ColorGradeImage = "colorgradelut256x16-70"
	if ColorLUT = 9 Then Table1.ColorGradeImage = "colorgradelut256x16-80"
	if ColorLUT = 10 Then Table1.ColorGradeImage = "colorgradelut256x16-90"
	if ColorLUT = 11 Then Table1.ColorGradeImage = "colorgradelut256x16-100"

	CabRails = Table1.Option("Cabinet Rails Visible", 1, 2, 1, 1, 0, Array("True", "False"))
	If CabRails = 1 Then
		For Each BPRails in BP_PincabRails : BPRails.visible = 1: Next
		Ramp1.visible = False
		Ramp2.visible = False
	Else
		For Each BPRails in BP_PincabRails : BPRails.visible = 0: Next
		Ramp1.visible = True
		Ramp2.visible = True
	End If

    VRRoom = Table1.Option("VR Room", 1, 3, 1, 3, 0, Array("Pool Hall", "Minimal", "Basketball Court"))
	SetupVRRoom

	VRSound = Table1.Option("Background Audio", 1, 3, 1, VRSoundInit, 0, Array("Basketball", "Theme Song", "None"))
	SetupSound

	VRSoundVolume = Table1.Option("Background Audio Volume", 0, 1, 0.01, 0.05, 1)

    ' Sound volumes
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)

	' Room brightness
	LightLevel = NightDay/100
	SetRoomBrightness LightLevel   'Uncomment this line for lightmapped tables.

	If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub

Sub SetupSound()
	If VRSound = 1 Then
		StopSound "Song"
		PlaySound "basketball_exterior", -1, VRSoundVolume
	ElseIf VRSound = 2 Then
		StopSound "basketball_exterior"
		PlaySound "Song", -1, VRSoundVolume
	Else
		StopSound "Song"
		StopSound "basketball_exterior"
	End If
End Sub

Dim VRBGObj, VRLed

Sub SetBackglass()
	If RenderingMode = 2 Or LiveViewVRSim = 1 Then
		For Each VRBGObj In VRBackglass
			VRBGObj.x = VRBGObj.x
			VRBGObj.height = - VRBGObj.y + 140
			VRBGObj.y = 165 'adjusts the distance from the backglass towards the user
			VRBGObj.Rotx = -90
		Next
	Else
		For Each VRBGObj In VRBackglass : VRBGObj.visible = 0 : Next
		For Each VRLed in VRLeds : VRLed.opacity = 0 : Next
	End If
End Sub

Sub SetupVRRoom()
	If RenderingMode = 2 Or LiveViewVRSim = 1 Then

		For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
		
	
		TRKickerPrim1.Visible = 1
		TRKickerPrim2.Visible = 1
		Flasher1.Visible = 0
		Flasher2.Visible = 0
		rrail.Visible = 0
		BM_PincabRails.Visible = 0
		lrail.Visible = 0
		Table1.PlayfieldReflectionStrength = 8
		FlBGL001.opacity = 100
		FlBGL29.opacity = 100
		FlBGL27.opacity = 100
		FlBGL45.opacity = 100
		FlBGL13.opacity = 100
		FlBGL43.opacity = 100
		FlBGL61.opacity = 100
	

		For Each BPRails in BP_PincabRails : BPRails.visible = 0: Next
		For Each BPRamp1 in BP_ramp1 : BPRamp1.visible = 0: Next
		For Each BPRamp2 in BP_ramp2 : BPRamp2.visible = 0: Next
	
		If VRRoom = 3 Then
			For Each VR_Obj in VRPoolHall : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VRSphereRoom : VR_Obj.Visible = 1 : Next
		ElseIf VRRoom = 2 Then
			For Each VR_Obj in VRPoolHall : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
			For Each VR_Obj in VRSphereRoom : VR_Obj.Visible = 0 : Next
		Else
			For Each VR_Obj in VRPoolHall : VR_Obj.Visible = 1 : Next
			For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VRSphereRoom : VR_Obj.Visible = 0 : Next
		End If
	
	Else
		VRRoom = 0
		FlBGL001.opacity = 0
		FlBGL29.opacity = 0
		FlBGL27.opacity = 0
		FlBGL45.opacity = 0
		FlBGL13.opacity = 0
		FlBGL43.opacity = 0
		FlBGL61.opacity = 0
		For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VRPoolHall : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VRSphereRoom : VR_Obj.Visible = 0 : Next
	
	End If
End Sub

Sub TimerVRPlunger_Timer
	If Pincab_Plunger.Y < 2114 then
		Pincab_Plunger.Y = Pincab_Plunger.Y + 5
	End If
End Sub

Sub TimerVRPlunger2_Timer
	Pincab_Plunger.Y = 2114 + (5* Plunger.Position) - 20
End Sub

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim x

'Const cGameName = "hglbtrtr" ' normal 6 digits rom
Const cGameName = "hglbtrtb" ' bootleg 7 digits rom

Const UseSolenoids = 2
Const UseLamps = 1
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

Dim VarHidden
If Table1.ShowDT = true then
    VarHidden = 1
    For each x in aReels
        x.Visible = 1
    Next
else
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
end if

if B2SOn = true then VarHidden = 1

LoadVPM "03060000", "bally.vbs", 3.02


'**********************************
' 	ZMAT: General Math Functions
'**********************************

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function



'************
' Table init.
'************

Dim HGBall1, gBOT

Sub table1_Init
    vpmInit me

    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Harlem Globetrotters on Tour - Bally 1979"
        .Games(cGameName).Settings.Value("sound") = 1
	   .Games(cGameName).Settings.Value("volume")= 1
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot, Sling1, Sling2)

    Set HGBall1 = Drain.CreateSizedballWithMass(Ballsize/2,Ballmass)
    Controller.Switch(8) = 1
    gBOT = Array(HGBall1)

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Turn on Gi
    GIOn()

	vpmMapLights allLamps ' Map all lamps into lights array

	Setup_Backglass()

	Dim BPLSlingInit
	Dim BPRSlingInit
	For Each BPLSlingInit in BP_LeftSling2 : BPLSlingInit.visible = 0: Next
	For Each BPLSlingInit in BP_LeftSling3 : BPLSlingInit.visible = 0: Next
	For Each BPLSlingInit in BP_LeftSling4 : BPLSlingInit.visible = 0: Next

	For Each BPRSlingInit in BP_RightSling2 : BPRSlingInit.visible = 0: Next
	For Each BPRSlingInit in BP_RightSling3 : BPRSlingInit.visible = 0: Next
	For Each BPRSlingInit in BP_RightSling4 : BPRSlingInit.visible = 0: Next

	SetupVRRoom
	SetBackglass
	SetupSound

End Sub



'******************************************************
' 	ZBBR: BALL BRIGHTNESS
'******************************************************

Const BallBrightness =  .1      	'Ball brightness - Value between 0 and 1 (0=Dark ... 1=Bright)

' Constants for plunger lane ball darkening.
' You can make a temporary wall in the plunger lane area and use the co-ordinates from the corner control points.
Const PLOffset = 0.5			'Minimum ball brightness scale in plunger lane
Const PLLeft = 1000 			'X position of punger lane left
Const PLRight = 1060 			'X position of punger lane right
Const PLTop = 1225 				'Y position of punger lane top
Const PLBottom = 1900 			'Y position of punger lane bottom
Dim PLGain: PLGain = (1-PLOffset)/(PLTop-PLBottom)

Sub UpdateBallBrightness
	Dim s, b_base, b_r, b_g, b_b, d_w
	b_base = 120 * BallBrightness + 135*gilvl	' orig was 120 and 70

	For s = 0 To UBound(gBOT)
		' Handle z direction
		d_w = b_base*(1 - (gBOT(s).z-25)/500)
		If d_w < 30 Then d_w = 30
		' Handle plunger lane
		If InRect(gBOT(s).x,gBOT(s).y,PLLeft,PLBottom,PLLeft,PLTop,PLRight,PLTop,PLRight,PLBottom) Then  
			d_w = d_w*(PLOffset+PLGain*(gBOT(s).y-PLBottom))
		End If
		' Assign color
		b_r = Int(d_w)
		b_g = Int(d_w)
		b_b = Int(d_w)
		If b_r > 255 Then b_r = 255
		If b_g > 255 Then b_g = 255
		If b_b > 255 Then b_b = 255
		gBOT(s).color = b_r + (b_g * 256) + (b_b * 256 * 256)
		'debug.print "--- ball.color level="&b_r
	Next
End Sub


'****************************
' 	ZRBR: Room Brightness
'****************************

'This code only applies to lightmapped tables. It is here for reference. 
'NOTE: Objects bightness will be affected by the Day/Night slider only if their blenddisablelighting property is less than 1.
'      Lightmapped table primitives have their blenddisablelighting equal to 1, therefore we need this SetRoomBrightness sub 
'      to handle updating their effective ambient brighness.

' Update these arrays if you want to change more materials with room light level
Dim RoomBrightnessMtlArray: RoomBrightnessMtlArray = Array("VLM.Bake.Active","VLM.Bake.Solid") 

Sub SetRoomBrightness(lvl)
	If lvl > 1 Then lvl = 1
	If lvl < 0 Then lvl = 0

	' Lighting level
	Dim v: v=(lvl * 245 + 10)/255

	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		ModulateMaterialBaseColor RoomBrightnessMtlArray(i), i, v
	Next
End Sub

Dim SavedMtlColorArray
SaveMtlColors
Sub SaveMtlColors
	ReDim SavedMtlColorArray(UBound(RoomBrightnessMtlArray))
	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		SaveMaterialBaseColor RoomBrightnessMtlArray(i), i
	Next
End Sub

Sub SaveMaterialBaseColor(name, idx)
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	SavedMtlColorArray(idx) = round(base,0)
End Sub


Sub ModulateMaterialBaseColor(name, idx, val)
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	Dim red, green, blue, saved_base, new_base
 
	'First get the existing material properties
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle

	'Get saved color
	saved_base = SavedMtlColorArray(idx)
    
	'Next extract the r,g,b values from the base color
	red = saved_base And &HFF
	green = (saved_base \ &H100) And &HFF
	blue = (saved_base \ &H10000) And &HFF
	'msgbox red & " " & green & " " & blue

	'Create new color scaled down by 'val', and update the material
	new_base = RGB(red*val, green*val, blue*val)
	UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, new_base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

'**********
' Keys
'**********

Dim BIPL : BIPL = False				'Ball in plunger lane

Sub table1_KeyDown(ByVal Keycode)
    If Keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress : FlipperActivate LeftFlipper1, LFPress1: PinCab_FlipperLeft.X = PinCab_FlipperLeft.X + 10: End If
    If Keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress : PinCab_FlipperRight.X = PinCab_FlipperRight.X - 10: End If
    If KeyCode = KeyUpperLeft Then FlipperActivate LeftFlipper1, LFPress1 : PinCab_StartButton.y = PinCab_StartButton.Y - 5: End If
    If keycode = PlungerKey Then
		Plunger.Pullback
		SoundPlungerPull
		TimerVRPlunger.Enabled = True
		TimerVRPlunger2.Enabled = False
	End If
    If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft: End If			' Sets the nudge angle and power
    If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight: End If		' ^
    If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter: End If		' ^
    If keycode = StartGameKey Then SoundStartButton : PinCab_StartButton.y = PinCab_StartButton.Y - 5
    If keycode = AddCreditKey or keycode = AddCreditKey2 Then
        Select Case Int(rnd*3)
            Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
            Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
            Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
        End Select
    End If

    If vpmKeyDown(keycode) Then Exit Sub

End Sub

Sub table1_KeyUp(ByVal Keycode)

    If Keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress : FlipperDeActivate LeftFlipper1, LFPress1 : PinCab_FlipperLeft.X = PinCab_FlipperLeft.X - 10: End If
	If Keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress : PinCab_FlipperRight.X = PinCab_FlipperRight.X +10: End If
    If KeyCode = KeyUpperLeft Then FlipperDeActivate LeftFlipper1, LFPress1 : End If
	If Keycode = StartGameKey Then PinCab_StartButton.y = PinCab_StartButton.y + 5: End If
	If keycode = PlungerKey Then 
		Plunger.Fire
		SoundPlungerReleaseBall
		TimerVRPlunger.Enabled = False
		TimerVRPlunger2.Enabled = True
	End If

    If vpmKeyUp(keycode) Then Exit Sub

End Sub

'*********
' Switches
'*********

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)	
	RandomSoundSlingshotLeft BM_SlingArm2
    LStep = 0
    vpmTimer.PulseSw 37
	LeftSlingShot.TimerInterval = 17
    LeftSlingShot.TimerEnabled = 1

	Dim BP
	For Each BP in BP_LeftSling4 : BP.visible = 1: Next
	For Each BP in BP_LeftSling1 : BP.visible = 0: Next
	For Each BP in BP_SlingArm2 : BP.rotx = 18: Next

End Sub

Sub LeftSlingShot_Timer
	Dim BP
	Select Case LStep
	Case 3:	
		For Each BP in BP_LeftSling4 : BP.visible = 0: Next
		For Each BP in BP_LeftSling3 : BP.visible = 1: Next
		For Each BP in BP_SlingArm2 : BP.rotx = 12: Next
	Case 4:
		For Each BP in BP_LeftSling3 : BP.visible = 0: Next
		For Each BP in BP_LeftSling2 : BP.visible = 1: Next
		For Each BP in BP_SlingArm2 : BP.rotx = 6: Next
	Case 5:
		For Each BP in BP_LeftSling2 : BP.visible = 0: Next
		For Each BP in BP_LeftSling1 : BP.visible = 1: Next
		For Each BP in BP_SlingArm2 : BP.rotx = 0: Next
		LeftSlingShot.TimerEnabled = 0
	End Select

	LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)	
	RandomSoundSlingshotRight BM_SlingArm1
    RStep = 0
    vpmTimer.PulseSw 36
	RightSlingShot.TimerInterval = 17
    RightSlingShot.TimerEnabled = 1

	Dim BP
	For Each BP in BP_RightSling4 : BP.visible = 1: Next
	For Each BP in BP_RightSling1 : BP.visible = 0: Next
	For Each BP in BP_SlingArm1 : BP.rotx = 18: Next
End Sub

Sub RightSlingShot_Timer
	Dim BP

	Select Case RStep
		Case 3:	
			For Each BP in BP_RightSling4 : BP.visible = 0: Next
			For Each BP in BP_RightSling3 : BP.visible = 1: Next
			For Each BP in BP_SlingArm1 : BP.rotx = 12: Next
		Case 4:
			For Each BP in BP_RightSling3 : BP.visible = 0: Next
			For Each BP in BP_RightSling2 : BP.visible = 1: Next
			For Each BP in BP_SlingArm1 : BP.rotx = 6: Next
		Case 5:
			For Each BP in BP_RightSling2 : BP.visible = 0: Next
			For Each BP in BP_RightSling1 : BP.visible = 1: Next
			For Each BP in BP_SlingArm1 : BP.rotx = 0: Next
			RightSlingShot.TimerEnabled = 0
	End Select

	RStep = RStep + 1
End Sub

Sub RubberBand20_Hit:vpmTimer.PulseSw 34:End Sub
Sub RubberBand21_Hit:vpmTimer.PulseSw 34:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 40:RandomSoundBumperTop bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 38:RandomSoundBumperMiddle bumper2:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 39:RandomSoundBumperBottom bumper3:End Sub

Sub Bumper1_Animate
	Dim z, BL
	z = Bumper1.CurrentRingOffset
	For Each BL in BP_Bumper1_ring : BL.transz = z: Next
End Sub

Sub Bumper2_Animate
	Dim z, BL
	z = Bumper2.CurrentRingOffset
	For Each BL in BP_Bumper2_ring : BL.transz = z: Next
End Sub

Sub Bumper3_Animate
	Dim z, BL
	z = Bumper3.CurrentRingOffset
	For Each BL in BP_Bumper3_ring : BL.transz = z: Next
End Sub

Dim Bumpers : Bumpers = Array(Bumper1, Bumper2, Bumper3)

Sub AnimateBumperSkirts
	dim r, g, s, x, y, b, z
	' Animate Bumper switch (experimental)
	For r = 0 To 2
		g = 10000.
		For s = 0 to UBound(gBOT)
			If gBOT(s).z < 30 Then ' Ball on playfield
				x = Bumpers(r).x - gBOT(s).x
				y = Bumpers(r).y - gBOT(s).y
				b = x * x + y * y
				If b < g Then g = b
			End If
		Next
		z = 4
		If g < 80 * 80 Then
			z = 1
		End If
		If r = 0 Then For Each x in BP_Bumper1_Socket: x.Z = z: Next
		If r = 1 Then For Each x in BP_Bumper2_Socket: x.Z = z: Next
		If r = 2 Then For Each x in BP_Bumper3_Socket: x.Z = z: Next
	Next
End Sub

' Drain & holes & Kickers
Sub Drain_Hit:Controller.Switch(8) = 1: RandomSoundDrain drain: End Sub
Sub Drain_UnHit : Controller.Switch(8) = 0 : End Sub
Sub sw24_Hit:Controller.Switch(24) = 1: SoundSaucerLock :End Sub
Sub sw32_Hit:Controller.Switch(32) = 1: SoundSaucerLock :End Sub


' Gates
Sub Gate2_Animate
	Dim a : a = Gate2.CurrentAngle
	Dim BL : For Each BL in BP_Gate2 : BL.rotx = a: Next
End Sub

Sub Gate3_Animate
	Dim a : a = Gate3.CurrentAngle
	Dim BL : For Each BL in BP_Gate3 : BL.rotx = a: Next
End Sub

Sub Gate4_Animate
	Dim a : a = Gate4.CurrentAngle
	Dim BL : For Each BL in BP_Gate4 : BL.rotx = a: Next
End Sub

' Rollovers
Sub sw31_Hit:Controller.Switch(31) = 1:RandomSoundRollover:End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub

Sub sw22_Hit:Controller.Switch(22) = 1:RandomSoundRollover:End Sub
Sub sw22_UnHit:Controller.Switch(22) = 0:End Sub

Sub sw23_Hit:Controller.Switch(23) = 1:RandomSoundRollover:End Sub
Sub sw23_UnHit::Controller.Switch(23) = 0:End Sub

Sub sw23_Animate()
	Dim z : z = sw23.CurrentAnimOffset
	Dim BL : For Each BL in BP_Rollover_sw23 : BL.transz = z: Next
End Sub

Sub sw22_Animate()
	Dim z : z = sw22.CurrentAnimOffset
	Dim BL : For Each BL in BP_Rollover_sw22 : BL.transz = z: Next
End Sub

Sub sw31_Animate()
	Dim z : z = sw23.CurrentAnimOffset
	Dim BL : For Each BL in BP_Rollover_sw31 : BL.transz = z: Next
End Sub

'Spinners
Sub sw17_Spin():vpmTimer.PulseSw 17:SoundSpinner sw17:End Sub
Sub sw33_Spin():vpmTimer.PulseSw 33:SoundSpinner sw33:End Sub
Sub sw25_Spin():vpmTimer.PulseSw 25:SoundSpinner sw25:End Sub

BM_Spinner_sw17W.Visible = 1  'This is the spinner axis wire
BM_Spinner_sw17A.Visible = 1  'This is the spinner rest position, and is inner russian doll
BM_Spinner_sw17B.Visible = 0  'This is the spinner 180 position, and is outer russian doll

Sub sw17_FrameAnimate
    Dim BP, a, b, offset
    a = sw17.currentangle
    If a >= 0 And a < 60 Then
        b = 0
    ElseIf a >= 60 And a < 120 Then
        b = (a - 60) / 60
    ElseIf a >= 120 And a < 240 Then
        b = 1
    ElseIf a >= 240 And a < 300 Then
        b = 1 + (240 - a) / 60
    Else
        b = 0
    End If
    For Each BP in BP_Spinner_sw17W: BP.RotX = a: Next
    For Each BP in BP_Spinner_sw17A
        BP.RotX = a
        BP.Opacity = 100 * (1 - b)
    Next
    For Each BP in BP_Spinner_sw17B
        BP.RotX = a
        BP.Opacity = 100 * b
    Next
    For Each BP in BP_Spinner_sw17_Wire
        offset = b * 7.87
        BP.transy = (Sin((a+180) * (2*PI/360)) * 5) + 1.5
        BP.transz = (Sin((a-90) * (2*PI/360)) * 5) + 5 + offset
    Next
End Sub

BM_Spinner_sw25W.Visible = 1  'This is the spinner axis wire
BM_Spinner_sw25A.Visible = 1  'This is the spinner rest position, and is inner russian doll
BM_Spinner_sw25B.Visible = 0  'This is the spinner 180 position, and is outer russian doll

Sub sw25_FrameAnimate
    Dim BP, a, b, offset
    a = sw25.currentangle
    If a >= 0 And a < 60 Then
        b = 0
    ElseIf a >= 60 And a < 120 Then
        b = (a - 60) / 60
    ElseIf a >= 120 And a < 240 Then
        b = 1
    ElseIf a >= 240 And a < 300 Then
        b = 1 + (240 - a) / 60
    Else
        b = 0
    End If
    For Each BP in BP_Spinner_sw25W: BP.RotX = a: Next
    For Each BP in BP_Spinner_sw25A
        BP.RotX = a
        BP.Opacity = 100 * (1 - b)
    Next
    For Each BP in BP_Spinner_sw25B
        BP.RotX = a
        BP.Opacity = 100 * b
    Next
    For Each BP in BP_Spinner_sw25_Wire
        offset = b * 7.87
        BP.transy = (Sin((a+180) * (2*PI/360)) * 5) + 1.5
        BP.transz = (Sin((a-90) * (2*PI/360)) * 5) + 5 + offset
    Next
End Sub

BM_Spinner_sw33W.Visible = 1  'This is the spinner axis wire
BM_Spinner_sw33A.Visible = 1  'This is the spinner rest position, and is inner russian doll
BM_Spinner_sw33B.Visible = 0  'This is the spinner 180 position, and is outer russian doll

Sub sw33_FrameAnimate
    Dim BP, a, b, offset
    a = sw33.currentangle
    If a >= 0 And a < 60 Then
        b = 0
    ElseIf a >= 60 And a < 120 Then
        b = (a - 60) / 60
    ElseIf a >= 120 And a < 240 Then
        b = 1
    ElseIf a >= 240 And a < 300 Then
        b = 1 + (240 - a) / 60
    Else
        b = 0
    End If
    For Each BP in BP_Spinner_sw33W: BP.RotX = a: Next
    For Each BP in BP_Spinner_sw33A
        BP.RotX = a
        BP.Opacity = 100 * (1 - b)
    Next
    For Each BP in BP_Spinner_sw33B
        BP.RotX = a
        BP.Opacity = 100 * b
    Next
    For Each BP in BP_Spinner_sw33_Wire
        offset = b * 7.87
        BP.transy = (Sin((a+180) * (2*PI/360)) * 5) + 1.5
        BP.transz = (Sin((a-90) * (2*PI/360)) * 5) + 5 + offset
    Next
End Sub

'Leafs
Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub
Sub sw34a_Hit:vpmTimer.PulseSw 134:End Sub
 
' Stand up Targets
Sub sw26o_Hit
	TargetBouncer Activeball, 1
End Sub
Sub sw27o_Hit
	TargetBouncer Activeball, 1
End Sub
Sub sw28o_Hit
	TargetBouncer Activeball, 1
End Sub
Sub sw29o_Hit
	TargetBouncer Activeball, 1
End Sub
Sub sw30o_Hit
	TargetBouncer Activeball, 1
End Sub
Sub sw35o_Hit
	TargetBouncer Activeball, 1
End Sub


Sub sw35_Hit:vpmTimer.PulseSw 35: STHit 35 :End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26: STHit 26 :End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27: STHit 27 :End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28: STHit 28 :End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29: STHit 29 :End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30: STHit 30 :End Sub

' Drop Targets
Sub sw1_hit
	DTHit 1
End Sub

Sub sw2_hit
	DTHit 2
End Sub

Sub sw3_hit
	DTHit 3
End Sub

Sub sw4_hit
	DTHit 4
End Sub

'*********
'Solenoids
'*********

SolCallback(7) = "SolBallRelease"
SolCallback(6) = "vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker),"
SolCallBack(13) = "SolTopSaucer"
SolCallBack(14) = "SolRightSaucer"
SolCallback(15) = "dtDropreset"
SolCallback(19) = "vpmNudge.SolGameOn"
SolCallback(17) = "Soldiv"

Sub Soldiv(Enabled)
    vpmSolDiverter RightLaneGate, True, Not Enabled
End Sub

Sub dtDropreset(enabled)
	If enabled Then 
		DTRaise 1
		DTRaise 2
		DTRaise 3
		DTRaise 4
		RandomSoundDropTargetReset BM_DTsw1
	End If
End Sub

Sub SolBallRelease(enabled)
    If enabled Then
        Drain.kick 57, 20
        RandomSoundBallRelease Drain
    End If
End Sub

Sub SolTopSaucer(enabled)
	Dim KickerVariance : KickerVariance = Int(rnd*8)
    If enabled Then
        sw24.kick (200 + KickerVariance), 10
        SoundSaucerKick 1, sw24
        Controller.Switch(24) = 0
    End If
End Sub

Sub SolRightSaucer(enabled) 
    If enabled Then
        sw32.kick -45, 20  
        SoundSaucerKick 1, sw32
        Controller.Switch(32) = 0
    End If
End Sub


'*******************
' Flipper Subs v3.0
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sULFlipper) = "SolLFlipper1"

'*******************************************
'  Flippers
'*******************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolLFlipper1(Enabled)
	If Enabled Then
		LF1.Fire
		If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper1
		Else 
			SoundFlipperUpAttackLeft LeftFlipper1
			RandomSoundFlipperUpLeft LeftFlipper1
		End If		
	Else
		LeftFlipper1.RotateToStart
		If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper1
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

Sub LeftFlipper1_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper1, LFCount1, parm
	LF1.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub LeftFlipper_Animate
	Dim a : a = LeftFlipper.CurrentAngle

	FlipperLSh.RotZ = LeftFlipper.CurrentAngle

	Dim v, BP
	v = 255.0 * (122.0 - LeftFlipper.CurrentAngle) / (122.0 -  74.5)

	For each BP in BP_LFlipper
		BP.Rotz = a
		BP.visible = v < 128.0
	Next
	For each BP in BP_LFlipperU
		BP.Rotz = a
		BP.visible = v >= 128.0
	Next
End Sub

Sub RightFlipper_Animate
	Dim a : a = RightFlipper.CurrentAngle

	FlipperRSh.RotZ = RightFlipper.CurrentAngle

	Dim v, BP
	v = 255.0 * (-122.0 - RightFlipper.CurrentAngle) / (-122.0 + 74.5)

	For each BP in BP_RFlipper
		BP.Rotz = a
		BP.visible = v < 128.0
	Next
	For each BP in BP_RFlipperU
		BP.Rotz = a
		BP.visible = v >= 128.0
	Next
End Sub

Sub LeftFlipper1_Animate
	Dim a : a = LeftFlipper1.CurrentAngle

	FlipperLSh2.RotZ = LeftFlipper1.CurrentAngle

	Dim v, BP
	v = 255.0 * (122.0 - LeftFlipper1.CurrentAngle) / (122.0 -  74.5)

	For each BP in BP_LFlipper1
		BP.Rotz = a
		BP.visible = v < 128.0
	Next
	For each BP in BP_LFlipper1U
		BP.Rotz = a
		BP.visible = v >= 128.0
	Next
End Sub

'************************************
'          LEDs Display
'     Based on Scapino's LEDs
'************************************

Dim Digits(32)
Dim Patterns(11)
Dim Patterns2(11)

Patterns(0) = 0     'empty
Patterns(1) = 63    '0
Patterns(2) = 6     '1
Patterns(3) = 91    '2
Patterns(4) = 79    '3
Patterns(5) = 102   '4
Patterns(6) = 109   '5
Patterns(7) = 125   '6
Patterns(8) = 7     '7
Patterns(9) = 127   '8
Patterns(10) = 111  '9

Patterns2(0) = 128  'empty
Patterns2(1) = 191  '0
Patterns2(2) = 134  '1
Patterns2(3) = 219  '2
Patterns2(4) = 207  '3
Patterns2(5) = 230  '4
Patterns2(6) = 237  '5
Patterns2(7) = 253  '6
Patterns2(8) = 135  '7
Patterns2(9) = 255  '8
Patterns2(10) = 239 '9

'Assign 7-digit output to reels
Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5
Set Digits(6) = a6

Set Digits(7) = b0
Set Digits(8) = b1
Set Digits(9) = b2
Set Digits(10) = b3
Set Digits(11) = b4
Set Digits(12) = b5
Set Digits(13) = b6

Set Digits(14) = c0
Set Digits(15) = c1
Set Digits(16) = c2
Set Digits(17) = c3
Set Digits(18) = c4
Set Digits(19) = c5
Set Digits(20) = c6

Set Digits(21) = d000
Set Digits(22) = d001
Set Digits(23) = d002
Set Digits(24) = d003
Set Digits(25) = d004
Set Digits(26) = d005
Set Digits(27) = d006

Set Digits(28) = e0
Set Digits(29) = e1
Set Digits(30) = e2
Set Digits(31) = e3

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
            Next
        Next
    End IF
End Sub


Sub GiON
	For each x in aGiLights
		x.State = 1
	Next
End Sub

Sub GiOFF
	For each x in aGiLights
		x.State = 0
	Next
End Sub

Dim OldGiState
OldGiState = -1 'start witht he Gi off

Sub GIUpdate
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            GiOn
        End If
    End If
End Sub

'*************************************
'Bally Harlem Globetrotters 7 digits
'by Inkochnito
'*************************************
Sub editDips
    Dim vpmDips:Set vpmDips = New cvpmDips
    With vpmDips
        .AddForm 700, 400, "Harlem GlobeTrotters 7 digits - DIP switches"
        .AddFrame 2, 0, 190, "Maximum credits", &H03000000, Array("10 credits", 0, "15 credits", &H01000000, "25 credits", &H02000000, "free play (40 credits)", &H03000000)                    'dip 25&26
        .AddFrame 2, 87, 190, "Sound features", &H30000000, Array("chime effects", 0, "noises and no background", &H10000000, "noise effects", &H20000000, "noises and background", &H30000000) 'dip 29&30
        .AddFrame 2, 170, 190, "Special adjustment", &H00000060, Array("points", 0, "extra ball", &H00000040, "replay/extra ball", &H00000060)                                                  'dip 6&7
        .AddFrame 2, 230, 190, "5 side target lite adjustment", &H00800000, Array("targets will reset", 0, "targets are held in memory", &H00800000)                                            'dip 24
        .AddFrame 2, 276, 190, "G-L-O-B-E saucer scanning adjustment", &H00002000, Array("Globe lites do not scan", 0, "Globe lites keep scanning", &H00002000)                                 'dip 14
        .AddFrame 2, 322, 190, "Dunk shot target special", &H00400000, Array("is reset after collecting", 0, "stays lit", &H00400000)                                                           'dip 23
        .AddFrame 205, 0, 190, "High game to date", &H00200000, Array("no award", 0, "3 credits", &H00200000)                                                                                   'dip 22
        .AddFrame 205, 46, 190, "Score version", &H00100000, Array("6 digit scoring", 0, "7 digit scoring", &H00100000)                                                                         'dip 21
        .AddFrame 205, 92, 190, "Balls per game", &H40000000, Array("3 balls", 0, "5 balls", &H40000000)                                                                                        'dip 31
        .AddFrame 205, 138, 190, "Saucer targets reset", &H00000080, Array("when ball enters target saucer", 0, "on next ball in play", &H00000080)                                             'dip 8
        .AddFrame 205, 184, 190, "Super bonus", &H00004000, Array("is reset every ball", 0, "is held in memory", &H00004000)                                                                    'dip 15
        .AddFrame 205, 230, 190, "Globe special lite adjustment", &H80000000, Array("left outlane 25K only lit", 0, "left outlane 25K and Globe special lit", &H80000000)                       'dip 32
        .AddFrame 205, 276, 190, "Left and right spinner adjust", 32768, Array("left spinner only which alternates", 0, "left and right spinner lite on", 32768)                                'dip 16
        .AddChk 205, 330, 180, Array("Match feature", &H08000000)                                                                                                                               'dip 28
        .AddChk 205, 350, 115, Array("Credits displayed", &H04000000)                                                                                                                           'dip 27
        .AddLabel 50, 370, 350, 20, "After hitting OK, press F3 to reset game with new settings."
        .ViewDips
    End With
End Sub
Set vpmShowDips = GetRef("editDips")


'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

' Put all the Post and Pin objects in dPosts collection. Make sure dPosts fires hit events.
Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

' This collection contains the bottom sling posts. They are not in the dPosts collection so that the TargetBouncer is not applied to them, but they should still have dampening applied
' If you experience airballs with posts or targets, consider adding them to this collection
Sub NoTargetBouncer_Hit
    RubbersD.dampen ActiveBall
End Sub

' Put all the Sleeve objects in dSleeves collection. Make sure dSleeves fires hit events.
Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class



'******************************************************
'****  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Flippers: 	https://www.youtube.com/watch?v=FWvM9_CdVHw
' Dampeners: 	https://www.youtube.com/watch?v=tqsxx48C6Pg
' Physics: 		https://www.youtube.com/watch?v=UcRMG-2svvE
'
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25) 
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
' 
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |



'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level well need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.  
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end 
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'


'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
dim LF1 : Set LF1 = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
Sub InitPolarity()
   dim x, a : a = Array(LF, RF, LF1)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
		x.DebugOn=False ' prints some info in debugger


        x.AddPt "Polarity", 0, 0, 0
        x.AddPt "Polarity", 1, 0.05, - 2.7
        x.AddPt "Polarity", 2, 0.16, - 2.7
        x.AddPt "Polarity", 3, 0.22, - 0
        x.AddPt "Polarity", 4, 0.25, - 0
        x.AddPt "Polarity", 5, 0.3, - 1
        x.AddPt "Polarity", 6, 0.4, - 2
        x.AddPt "Polarity", 7, 0.5, - 2.7
        x.AddPt "Polarity", 8, 0.65, - 1.8
        x.AddPt "Polarity", 9, 0.75, - 0.5
        x.AddPt "Polarity", 10, 0.81, - 0.5
        x.AddPt "Polarity", 11, 0.88, 0
        x.AddPt "Polarity", 12, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.15, 0.85
		x.AddPt "Velocity", 2, 0.2, 0.9
		x.AddPt "Velocity", 3, 0.23, 0.95
		x.AddPt "Velocity", 4, 0.41, 0.95
		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
    LF.SetObjects "LF", LeftFlipper, TriggerLF
    RF.SetObjects "RF", RightFlipper, TriggerRF
    LF1.SetObjects "LF1", LeftFlipper1, TriggerLF1
End Sub


'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF, LF1)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire()
		Flipper.RotateToEnd
		processballs
	End Sub
	
	Public Property Get Pos 'returns % position a ball. For debug stuff.
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper1, LFPress1, LFCount1, LFEndAngle1, LFState1
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper1, LFEndAngle1
	FlipperNudge LeftFlipper1, LFEndAngle1, LFEOSNudge1,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge1, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	'   Dim BOT
	'   BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub
	


'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, LFPress1, RFPress, LFCount,  LFCount1, RFCount
Dim LFState, LFState1, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle, LFEndAngle1

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState1 = 1
LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle1 = Leftflipper1.endangle
LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b', BOT
		'		BOT = GetBalls
		
		For b = 0 To UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= - 0.4 Then gBOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub


'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate 
' how to make these updates. But in summary the following needs to be updated:	
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub


'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub


'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Aapron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj			
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj		
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 	
End Sub


' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	BSUpdate
	DoDTAnim 						'handle drop target animations
	DoSTAnim						'handle stand up target animations
    UpdateBallBrightness
	UpdateStandupTargets
	UpdateDropTargets
	AnimateBumperSkirts
	sw17_FrameAnimate
	sw25_FrameAnimate
	sw33_FrameAnimate

	RollingUpdate
	Dim BP
	For Each BP in BP_Diverter : BP.RotZ = -RightLaneGate.CurrentAngle: Next
	GIUpdate

	If RenderingMode = 2 Or LiveViewVRSim = 1 Then
		DisplayTimer
	Else
		UpdateLeds
	End If
End Sub

'****************************************************************
'  Backglass lamps
'****************************************************************

Sub l13a_animate()
	l13.SetValue l13a.state
End Sub

Sub l27a_animate()
	l27.SetValue l27a.state
End Sub

Sub l29a_animate()
	l29.SetValue l29a.state
End Sub

Sub l45a_animate()
	l45.SetValue l45a.state
End Sub

Sub l61a_animate()
	l61.SetValue l61a.state
End Sub

'****************************************************************
'  GI
'****************************************************************
dim gilvl:gilvl = 1
Sub ToggleGI(Enabled)
	dim xx
	If enabled Then
		for each xx in GI:xx.state = 1:Next
		gilvl = 1
	Else
		for each xx in GI:xx.state = 0:Next	
		GITimer.enabled = True
		gilvl = 0
	End If
	Sound_GI_Relay enabled, bumper1
End Sub

Sub GITimer_Timer()
	me.enabled = False
	ToggleGI 1
End Sub

'***************************************************************
'	ZSHA: Ambient ball shadows
'***************************************************************

' For dynamic ball shadows, Check the "Raytraced ball shadows" box for the specific light. 
' Also make sure the light's z position is around 25 (mid ball)

'Ambient (Room light source)
Const AmbientBSFactor = 0.9    '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 0			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objBallShadow(7)

'Initialization
BSInit

Sub BSInit()
	Dim iii
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 3 + iii / 1000
		objBallShadow(iii).visible = 0
	Next
End Sub


Sub BSUpdate
	Dim s: For s = lob To UBound(gBOT)
		' *** Normal "ambient light" ball shadow
		
		'Primitive shadow on playfield, flasher shadow in ramps
		'** If on main and upper pf
		If gBOT(s).Z > 20 And gBOT(s).Z < 30 Then
			objBallShadow(s).visible = 1
			objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
			objBallShadow(s).Y = gBOT(s).Y + offsetY
			'objBallShadow(s).Z = gBOT(s).Z + s/1000 + 1.04 - 25	

		'** No shadow if ball is off the main playfield (this may need to be adjusted per table)
		Else
			objBallShadow(s).visible = 0
		End If
	Next
End Sub


'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'

' If Function RotPoint exist somewhere else in the script. Uncomment below if needed
'Function RotPoint(x,y,angle)
'    dim rx, ry
'   rx = x*dCos(angle) - y*dSin(angle)
'   ry = x*dSin(angle) + y*dCos(angle)
'    RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class

'******************************************************
'	ZRST: STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
	Private m_primary, m_prim, m_sw, m_animate
  
	Public Property Get Primary(): Set Primary = m_primary: End Property
	Public Property Let Primary(input): Set m_primary = input: End Property
  
	Public Property Get Prim(): Set Prim = m_prim: End Property
	Public Property Let Prim(input): Set m_prim = input: End Property
  
	Public Property Get Sw(): Sw = m_sw: End Property
	Public Property Let Sw(input): m_sw = input: End Property
  
	Public Property Get Animate(): Animate = m_animate: End Property
	Public Property Let Animate(input): m_animate = input: End Property
  
	Public default Function init(primary, prim, sw, animate)
	  Set m_primary = primary
	  Set m_prim = prim
	  m_sw = sw
	  m_animate = animate
  
	  Set Init = Me
	End Function
End Class

'Define a variable for each stand-up target
Dim ST26, ST27, ST28, ST29, ST30, ST35

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts

Set ST26 = (new StandupTarget)(sw26, BM_STsw26,26, 0)
Set ST27 = (new StandupTarget)(sw27, BM_STsw27,27, 0)
Set ST28 = (new StandupTarget)(sw28, BM_STsw28,28, 0)
Set ST29 = (new StandupTarget)(sw29, BM_STsw29,29, 0)
Set ST30 = (new StandupTarget)(sw30, BM_STsw30,30, 0)
Set ST35 = (new StandupTarget)(sw35, BM_STsw35,35, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST26, ST27, ST28, ST29, ST30, ST35)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		DTBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)
		If STArray(i).sw = switch Then
			STArrayID = i
			Exit Function
		End If
	Next
End Function

Function STCheckHit(aBall, target) 'Check if target is hit on it's face
	Dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		STCheckHit = 1
	Else
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i = 0 To UBound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime
	
	STAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If animate = 1 Then
		primary.collidable = 0
		prim.transy =  - STMaxOffset
		vpmTimer.PulseSw switch mod 100
		STAnimate = 2
		Exit Function
	ElseIf animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else
			STAnimate = 2
		End If
	End If
End Function

Sub UpdateStandupTargets
	dim BP, ty

    ty = BM_STsw26.transy
	For each BP in BP_STsw26 : BP.transy = ty: Next

    ty = BM_STsw27.transy
	For each BP in BP_STsw27 : BP.transy = ty: Next

    ty = BM_STsw28.transy
	For each BP in BP_STsw28 : BP.transy = ty: Next

    ty = BM_STsw29.transy
	For each BP in BP_STsw29 : BP.transy = ty: Next

    ty = BM_STsw30.transy
	For each BP in BP_STsw30 : BP.transy = ty: Next

    ty = BM_STsw35.transy
	For each BP in BP_STsw35 : BP.transy = ty: Next

End Sub

'******************************************************
'***	END STAND-UP TARGETS
'******************************************************
  
'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
	Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped
  
	Public Property Get Primary(): Set Primary = m_primary: End Property
	Public Property Let Primary(input): Set m_primary = input: End Property
  
	Public Property Get Secondary(): Set Secondary = m_secondary: End Property
	Public Property Let Secondary(input): Set m_secondary = input: End Property
  
	Public Property Get Prim(): Set Prim = m_prim: End Property
	Public Property Let Prim(input): Set m_prim = input: End Property
  
	Public Property Get Sw(): Sw = m_sw: End Property
	Public Property Let Sw(input): m_sw = input: End Property
  
	Public Property Get Animate(): Animate = m_animate: End Property
	Public Property Let Animate(input): m_animate = input: End Property
  
	Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
	Public Property Let IsDropped(input): m_isDropped = input: End Property
  
	Public default Function init(primary, secondary, prim, sw, animate, isDropped)
	  Set m_primary = primary
	  Set m_secondary = secondary
	  Set m_prim = prim
	  m_sw = sw
	  m_animate = animate
	  m_isDropped = isDropped
  
	  Set Init = Me
	End Function
End Class

'Define a variable for each drop target
Dim DT1, DT2, DT3, DT4

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Array slot for handling the animation instrucitons, set to 0
'						Values for animate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 
'   isDropped:			Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT1 = (new DropTarget)(sw1, sw1a, BM_DTsw1, 1, 0, False)
Set DT2 = (new DropTarget)(sw2, sw2a, BM_DTsw2, 2, 0, False)
Set DT3 = (new DropTarget)(sw3, sw3a, BM_DTsw3, 3, 0, False)
Set DT4 = (new DropTarget)(sw4, sw4a, BM_DTsw4, 4, 0, False)

Dim DTArray
DTArray = Array(DT1,DT2,DT3,DT4)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 80 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
	
	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.rotz, DTMass
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate =  - 1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 To UBound(DTArray)
		If DTArray(i).sw = switch Then
			DTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub DTBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim)
	Dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	Xintersect = (aBall.y - dtprim.y - Tan(bangle) * aball.x + Tan(rangle2) * dtprim.x) / (Tan(rangle2) - Tan(bangle))
	Yintersect = Tan(rangle2) * Xintersect + (dtprim.y - Tan(rangle2) * dtprim.x)
	
	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		If DTEnableBrick = 1 And  perpvel > DTBrickVel And DTBrickVel <> 0 And cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		DTCheckBrick = 4
	Else
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i
	For i = 0 To UBound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	Dim transz, switchid
	Dim animtime, rangle
	
	switchid = switch
	
	Dim ind
	ind = DTArrayID(switchid)
	
	rangle = prim.rotz * PI / 180
	
	DTAnimate = animate
	
	If animate = 0 Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If (animate = 1 Or animate = 4) And animtime < DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		DTAnimate = animate
		Exit Function
	ElseIf (animate = 1 Or animate = 4) And animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 1 'If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0 'updated by rothbauerw to account for edge case
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If
	
	If animate = 2 Then
		transz = (animtime - DTDropDelay) / DTDropSpeed * DTDropUnits *  - 1
		If prim.transz >  - DTDropUnits  Then
			prim.transz = transz
		End If
		
		prim.rotx = DTMaxBend * Cos(rangle) / 2
		prim.roty = DTMaxBend * Sin(rangle) / 2
		
		If prim.transz <= - DTDropUnits Then
			prim.transz =  - DTDropUnits
			secondary.collidable = 0
			DTArray(ind).isDropped = True 'Mark target as dropped
			controller.Switch(Switchid mod 100) = 1
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		End If
	End If
	
	If animate = 3 And animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
	ElseIf animate = 3 And animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If
	
	If animate =  - 1 Then
		transz = (1 - (animtime) / DTDropUpSpeed) * DTDropUnits *  - 1
		
		If prim.transz =  - DTDropUnits Then
			Dim b
			'Dim gBOT
			'gBOT = GetBalls
			
			For b = 0 To UBound(gBOT)
				If InRotRect(gBOT(b).x,gBOT(b).y,prim.x, prim.y, prim.rotz, - 25, - 10,25, - 10,25,25, - 25,25) And gBOT(b).z < prim.z + DTDropUnits + 25 Then
					gBOT(b).velz = 20
				End If
			Next
		End If
		
		If prim.transz < 0 Then
			prim.transz = transz
		ElseIf transz > 0 Then
			prim.transz = transz
		End If
		
		If prim.transz > DTDropUpUnits Then
			DTAnimate =  - 2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = GameTime
		End If
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = False 'Mark target as not dropped
		controller.Switch(Switchid mod 100) = 0
	End If
	
	If animate =  - 2 And animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay) / DTDropSpeed * DTDropUnits *  - 1 + DTDropUpUnits
		If prim.transz < 0 Then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0
			
			primary.collidable = 1
			secondary.collidable = 0
		End If
	End If
End Function

Function DTDropped(switchid)
	Dim ind
	ind = DTArrayID(switchid)
	
	DTDropped = DTArray(ind).isDropped
End Function

Sub UpdateDropTargets
	dim BP, tz, rx, ry

    tz = BM_DTsw1.transz
	rx = BM_DTsw1.rotx
	ry = BM_DTsw1.roty
	For each BP in BP_DTsw1: BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

    tz = BM_DTsw2.transz
	rx = BM_DTsw2.rotx
	ry = BM_DTsw2.roty
	For each BP in BP_DTsw2 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

    tz = BM_DTsw3.transz
	rx = BM_DTsw3.rotx
	ry = BM_DTsw3.roty
	For each BP in BP_DTsw3 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

    tz = BM_DTsw4.transz
	rx = BM_DTsw4.rotx
	ry = BM_DTsw4.roty
	For each BP in BP_DTsw4 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next
End Sub



'******************************************************
'****  END DROP TARGETS
'******************************************************


'******************************
' Setup VR Backglass
'******************************
Dim xoff,yoff1, yoff2, yoff3, yoff4, yoff5,yoff6, zoff,xrot,zscale, xcen,ycen

Sub Setup_Backglass()

	xoff = -30
	yoff1 = 162 ' this is where you adjust the forward/backward position for player 1 score
	yoff2 = 162 ' this is where you adjust the forward/backward position for player 2 score
	yoff3 = 162 ' this is where you adjust the forward/backward position for player 3 score
	yoff4 = 162' this is where you adjust the forward/backward position for player 4 score
	yoff5 = 162 ' this is where you adjust the forward/backward position for credits and ball in play
	zoff = 699
	xrot = -90

	center_digits()

end sub

Sub center_digits()

	Dim ix, xx, yy, yfact, xfact, xobj

	zscale = 0.0000001

	xcen = (130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

	for ix = 0 to 6
		For Each xobj In DigitsVR(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff1 

			If (yy < 0.) then
				yy = yy * -1
			End If

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 7 to 13
		For Each xobj In DigitsVR(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff2 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 14 to 20
		For Each xobj In DigitsVR(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff3 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 21 to 27
		For Each xobj In DigitsVR(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff4 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next

	for ix = 28 to 31
		For Each xobj In DigitsVR(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff5 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next


end sub

Dim DigitsVR(32)
DigitsVR(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6)
DigitsVR(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6)
DigitsVR(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6)
DigitsVR(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6)
DigitsVR(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6)
DigitsVR(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6)
DigitsVR(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6)

DigitsVR(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6)
DigitsVR(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6)
DigitsVR(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6)
DigitsVR(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6)
DigitsVR(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6)
DigitsVR(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6)
DigitsVR(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6)

DigitsVR(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006)
DigitsVR(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106)
DigitsVR(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206)
DigitsVR(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306)
DigitsVR(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406)
DigitsVR(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506)
DigitsVR(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606)

DigitsVR(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006)
DigitsVR(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106)
DigitsVR(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206)
DigitsVR(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306)
DigitsVR(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406)
DigitsVR(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506)
DigitsVR(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606)

DigitsVR(28) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306)
DigitsVR(29) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406)
DigitsVR(30) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506)
DigitsVR(31) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606)

dim DisplayColor

DisplayColor =  RGB(255,40,1)

Sub DisplayTimer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 32) then
              For Each obj In DigitsVR(num)
'                   If chg And 1 Then obj.visible=stat And 1
				   If chg And 1 Then FadeDisplay obj, stat And 1	
                   chg=chg\2 : stat=stat\2
              Next
			Else
			     end if
        Next
    End If
 End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 12
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 8
	End If
End Sub

Sub InitDigitsVR()
	dim tmp, x, obj
	for x = 0 to uBound(DigitsVR)
		if IsArray(DigitsVR(x) ) then
			For each obj in DigitsVR(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

InitDigitsVR

' ******************************************************************************************
'      LAMP CALLBACK for the 6 backglass flasher lamps (not the solenoid conrolled ones)
' ******************************************************************************************

Set LampCallback = GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps()
	If Controller.Lamp(13) = 0 Then: FlBGL13.visible=0: else: FlBGL13.visible=1 'Ball In Play
	If Controller.Lamp(45) = 0 Then: FlBGL27.visible=0: else: FlBGL27.visible=1 'Game Over
	If Controller.Lamp(29) = 0 Then: FlBGL29.visible=0: else: FlBGL29.visible=1 'High Score To Date
	If Controller.Lamp(45) = 0 Then: FlBGL45.visible=0: else: FlBGL45.visible=1 'Match
	If Controller.Lamp(61) = 0 Then: FlBGL61.visible=0: else: FlBGL61.visible=1 'Tilt
	If Controller.Lamp(43) = 0 Then: FlBGL43.visible=0: else: FlBGL43.visible=1 'Shoot Again
End Sub


'*****************************************************************************************************
' VR PLUNGER ANIMATION
'*****************************************************************************************************

Sub TimerPlunger_Timer
	If PinCab_Plunger.Y < 2249 then
		PinCab_Plunger.Y = PinCab_Plunger.Y + 5
	End If
End Sub

Sub TimerPlunger2_Timer
	PinCab_Plunger.Y = 2114 + (5* Plunger.Position) -20
End Sub




'*******************************************
' VPW CHANGE LOG
'*******************************************

' 1.15 mcarter78 - 1k bake, updated flipper physics, dampners, some sounds, flipper/plunger/slings strength, use vpm inserts, fixed GI and added missing leaf switches
' 1.16 mcarter78 - new playfield scan (thanks Clark Kent!), 2k batch, new playfield mesh with kicker bevels, add tweak UI menu & room/ball brightness, replace pegs/rubbers, some more material updates, fix target kicker angle
' 1.17 mcarter78 - fix desktop score display, tweak right kicker settings
' 1.18 mcarter78 - new mega VR room
' 1.19 apophis - Fixed flippers script and triggers. Lowered DN setting to 0.1. Darkened desktop backdrop.
' 1.20 mcarter78 - 3k batch - animated rollovers, bumper rings, gates, flippers, spinners
' 1.21 mcarter78 - uncheck static rendering for flippers & spinners
' 1.22 mcarter78 - uncheck static for rest of movables, animate slings, diverter, targets, bumper skirts, add black wall under playfield
' 1.23 mcarter78 - 2k batch with secondary objects for spinners, with intitial animation testing.  Added spinner rods and fixed gate animations.
' 1.24 apophis - 2k batch with spinner updates
' 1.25 mcarter78 - set BM_Layer1 and BM_Playfield to hide parts behind, fix gate 2 angle, fix sling arms positions, fix diverter animation 
' 1.26 mcarter78 - 2k batch with fixed plastics & plunger cover.
' 1.27 mcarter78 - Animate spinner rods, flip VR sphere hdri horizontal, use dummy lights to bring back DT backglass lamps, slight slingshot power adjustment
' 1.28 mcarter78 - reduce z value for plunger, reduce flippers elasticity falloff, make biff bars non-collidable
' 1.29 mcarter78 - remove reflection from VR cabinet, add refraction probe to plastics, continue working on spinner rod animations
' 1.30 mcarter78 - fix spinner rod animations (thx cyberpez & apophis!), add trough gate
' 1.31 mcarter78 - new VR basketball model and court texture
' 1.32 mcarter78 - new cabinet decals (thanks DaRdog!), added background sound to VR mega room
' 1.33 mcarter78 - New 2k batch
' 1.34 apophis - Test fix for the spinners. Only the far right spinner texture was fixed. Will do them all after final batch.
' 1.35 mcarter78 - fix VR room and sound options selection, reduce ball brightness and pf reflections on ball, fix playfield transparency, add option to hide cab rails
' 1.36 mcarter78 - walls to prevent ball sliding under plastics, slight flipper strength increase, replace song file with mp3
' 1.37 mcarter78 - Remove sidewall lightmaps from VR table, edit description
' 1.38 mcarter78 - Make background sounds work for everyone, move plunger down, remove duplicate sound files, hide VR backglass when not in VR, variable top saucer kick angle
' 1.39 mcarter78 - Spinner damping & animation adjustments, flipper physics adjustments
' 1.40 mcarter78 - 4k Batch
' 1.41 mcarter78 - New apron, hide remaining backglass flashers not in VR, add missing wall in shooter lane
' 1.42 apophis - Fixed spinner bakemaps textures. Fixed flicker on left spinner. Deleted unnecessary images from image manager. Deleted insert outline flasher obj. Set playfield_mesh z postion to 0. Fixed/cleaned-up amb ball and flipper shadows. Enabled some reflections.
' RC1 mcarter78 - Fixed credit light color, animate VR plunger, fix flipper BM/LM position, re-hide sidewalls in VR
' RC2 mcarter78 - Fix VR plunger animation
' RC3 mcarter78 - Better EXR compression to reduce file size
' RC4 mcarter78 - Fix sling arm rotation issues, flipper triggers and right flipper rotation
' RC5 mcarter78 - Add blackout rails for when cab rails are not visible, default cabinet POV (Thanks TryToTilt!)
' RC6 apophis - SlingArm2 animation fix. Desktop POV update. Added screenshot and updated table info. Final cleanup.
' Release 1.0


