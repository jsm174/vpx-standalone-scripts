'Iron Man Vault Edition (Stern 2014)
'https://www.ipdb.org/machine.cgi?id=6154
'
'---------------  VPW Proudly Present   ----------------
'   _____ _____   ____  _   _   __  __          _   _ 
'  |_   _|  __ \ / __ \| \ | | |  \/  |   /\   | \ | |
'    | | | |__) | |  | |  \| | |      |  /  \  |  \| |
'    | | |  _  /| |  | |     | | |\/| | / /\ \ | . ` |
'   _| |_| | \ \| |__| | |\  | | |  | |/ ____ \| |\  |
'  |_____|_|  \_\\____/|_| \_| |_|  |_/_/    \_\_| \_|
'
'					 VAULT EDITION
'-------------------------------------------------------
'
'			 ** Requires 10.7.2 to run **
'
'************************
' VPW Iron Man Mechanics
'************************
'Project Lead - Sixtoe
'Playfield & Plastic Scans - Bord
'Table Modeling - Sixtoe & Tomate
'Blender Toolkit - Niwak
'Blender Toolkit Work - Tomate & Sixtoe
'Lighting - Sixtoe, Niwak, Tomate & iaakki
'Fleep Sounds & Sound Scripting - Apophis & Sixtoe
'Iron Monger Toy - Dazz for the model & 3D scanning it, Flupper for the 3d model simplification.
'Whiplash & Warmachine Toys - EBisLit for the models, Dazz for 3D scanning, Tomate for the 3d model simplification.
'Plastic Redraws - Sixtoe & Embee
'Iron Monger Animation Code - DJRobX, Sixtoe, iaakki, apophis
'Various fixes and script assistance - apophis, iaakki, Bord, flux, fluffhead35, Niwak
'Iron Man VR Topper - Rawd
'Backglass Image - Wildman
'Backglass VR lighting - leojreimroc
'Testing - VPW Team
'
' All options are available from in game menu accessed by holding both left then right magnasave.
' ** This menu needs FlexDMD to be installed **
' - Light Level
' - Colour Saturation
' - Outpost Difficulty
' - Mech Volume
' - Ramp Volume
' - Ball Volume
' - Cabinet Mode (Hides Rails)
' - Incandescant GI (The original vault edition table uses pure white LEDs)
' - Dynamic Ball Shadows
' - Spotlight Shadows
' - Ambient Ball Shadows
'
'Version Log at end of script.

Option explicit
Randomize

Const TableVersion = "1.0"	'Table version (shown in option UI)

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'*******************************************
'  Constants and Global Variables
'*******************************************

Const BallSize = 50			'Ball size must be 50
Const BallMass = 1			'Ball mass must be 1
Const tnob = 4				'Total number of balls
Const lob = 0				'Total number of locked balls

Dim tablewidth: tablewidth = Table.width
Dim tableheight: tableheight = Table.height

' VRRoom set based on RenderingMode

' Internal DMD in Desktop Mode, using a textbox (must be called before LoadVPM)
Dim UseVPMDMD, VRRoom, DesktopMode
DesktopMode = Table.ShowDT
If RenderingMode = 2 Then VRRoom = VRRoomChoice + 1 Else VRRoom = 0
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode

Const UseVPMModSol = True

LoadVPM "02800000", "Sam.VBS", 3.54

'**********************
' Standard definitions
'**********************
'Const cGameName = "im_183ve"
Const cGameName = "im_185ve"	'-	Fastflips Enabled
'Const cGameName = "im_186ve"	'-	Fastflips Not Enabled
Const UseSolenoids = 1
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

'*******************************************
'  Timers
'*******************************************

Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
	DoSTAnim
End Sub

Sub FrameTimer_Timer()
	HideLightHelper

    If DynamicBallShadowsOn=1 Then DynamicBSUpdate 'update ball shadows

	Dim x, y

	'Update Flippers
	
    Dim lfa: lfa = LeftFlipper.currentangle
    FlipperLSh.RotZ = lfa
	LFLogo_BM_Dark_Room.RotZ = lfa ' VLM.Props;BM;1;LFLogo
	LFLogo_LM_Lit_Room.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_GI.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_flashers_l130.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_inserts_l3.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_inserts_l49.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_inserts_l50.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_inserts_l51.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_inserts_l52.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_inserts_l53.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_inserts_l55.RotZ = lfa ' VLM.Props;LM;1;LFLogo

    Dim rfa: rfa = RightFlipper.currentangle
    FlipperRSh.RotZ = rfa
	RFLogo_BM_Dark_Room.RotZ = rfa ' VLM.Props;BM;1;RFLogo
	RFLogo_LM_Lit_Room.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_GI.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_flashers_l130.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_inserts_l3.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_inserts_l49.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_inserts_l50.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_inserts_l51.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_inserts_l52.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_inserts_l53.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_inserts_l55.RotZ = rfa ' VLM.Props;LM;1;RFLogo

	'Update Gates
	
	Dim lrg1:lrg1 = LeftRampGate1.currentAngle + 90
	Gate01_BM_Dark_Room.RotX = lrg1 ' VLM.Props;BM;1;Gate01
	Gate01_LM_Lit_Room.RotX = lrg1 ' VLM.Props;LM;1;Gate01
	Gate01_LM_GI.RotX = lrg1 ' VLM.Props;LM;1;Gate01
	Gate01_LM_flashers_l127.RotX = lrg1 ' VLM.Props;LM;1;Gate01
	Gate01_LM_flashers_l131.RotX = lrg1 ' VLM.Props;LM;1;Gate01

	Dim lrg2:lrg2 = LeftRampGate2.currentAngle + 90
	gate02_BM_Dark_Room.RotX = lrg2 ' VLM.Props;BM;1;gate02
	gate02_LM_Lit_Room.RotX = lrg2 ' VLM.Props;LM;1;gate02
	gate02_LM_GI.RotX = lrg2 ' VLM.Props;LM;1;gate02
	gate02_LM_flashers_l121.RotX = lrg2 ' VLM.Props;LM;1;gate02

	Dim Rrg1:Rrg1 = RightRampGate1.currentAngle + 90
	gate03_BM_Dark_Room.RotX = Rrg1 ' VLM.Props;BM;1;gate03
	gate03_LM_Lit_Room.RotX = Rrg1 ' VLM.Props;LM;1;gate03
	gate03_LM_GI.RotX = Rrg1 ' VLM.Props;LM;1;gate03
	gate03_LM_flashers_l120.RotX = Rrg1 ' VLM.Props;LM;1;gate03
	gate03_LM_flashers_l126.RotX = Rrg1 ' VLM.Props;LM;1;gate03
	gate03_LM_flashers_l129.RotX = Rrg1 ' VLM.Props;LM;1;gate03
	gate03_LM_flashers_l130.RotX = Rrg1 ' VLM.Props;LM;1;gate03

	Dim Rrg2:Rrg2 = RightRampGate2.currentAngle + 90
	gate04_BM_Dark_Room.RotX = Rrg2 ' VLM.Props;BM;1;gate04
	gate04_LM_GI.RotX = Rrg2 ' VLM.Props;LM;1;gate04
	gate04_LM_flashers_l120.RotX = Rrg2 ' VLM.Props;LM;1;gate04
	gate04_LM_flashers_l126.RotX = Rrg2 ' VLM.Props;LM;1;gate04

	Dim G2:G2 = (GateCentre.currentAngle)
	gate05_BM_Dark_Room.RotX = G2 ' VLM.Props;BM;1;gate05
	gate05_LM_GI.RotX = G2 ' VLM.Props;LM;1;gate05
	gate05_LM_flashers_l120.RotX = G2 ' VLM.Props;LM;1;gate05
	gate05_LM_flashers_l123.RotX = G2 ' VLM.Props;LM;1;gate05
	gate05_LM_inserts_l9.RotX = G2 ' VLM.Props;LM;1;gate05

	'Update Spinners

	Dim spinLAngle:spinLAngle = -(sw11.currentAngle)
	Gate_SpinLeft_BM_Dark_Room.RotX = spinLAngle ' VLM.Props;BM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_Lit_Room.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_GI.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_flashers_l122.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_flashers_l125.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_flashers_l127.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_flashers_l129.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_flashers_l131.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_inserts_l16.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_inserts_l17.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft
	Gate_SpinLeft_LM_inserts_l27.RotX = spinLAngle ' VLM.Props;LM;1;Gate_SpinLeft

	SpinnerTShadowL.size_y = abs(sin( (spinLAngle+180) * (2*PI/360)) * 5)

	Dim spinMAngle:spinMAngle = -(sw13.currentAngle)
	Gate_SpinMid_BM_Dark_Room.RotX = spinMAngle ' VLM.Props;BM;1;Gate_SpinMid
	Gate_SpinMid_LM_Lit_Room.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_GI.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_flashers_l120.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_flashers_l121.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_flashers_l122.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_flashers_l123.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_flashers_l126.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_flashers_l127.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_flashers_l129.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_inserts_l9.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid
	Gate_SpinMid_LM_inserts_l10.RotX = spinMAngle ' VLM.Props;LM;1;Gate_SpinMid

	SpinnerTShadowM.size_y = abs(sin( (spinMAngle+180) * (2*PI/360)) * 5)

	Dim spinRAngle:spinRAngle = -(sw14.currentAngle)
	Gate_SpinRight_BM_Dark_Room.RotX = spinRAngle ' VLM.Props;BM;1;Gate_SpinRight
	Gate_SpinRight_LM_Lit_Room.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_GI.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_flashers_l122.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_flashers_l129.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_flashers_l132.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_inserts_l35.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_inserts_l37.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_inserts_l39.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_inserts_l40.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_inserts_l41.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_inserts_l42.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight
	Gate_SpinRight_LM_inserts_l63.RotX = spinRAngle ' VLM.Props;LM;1;Gate_SpinRight

	SpinnerTShadowR.size_y = abs(sin( (spinRAngle+180) * (2*PI/360)) * 5)

	' Sync targets lightmaps to the solid bake

	x = sw33_BM_Dark_Room.transx
	y = sw33_BM_Dark_Room.transy
	sw33_LM_Lit_Room.rotx = x ' VLM.Props;LM;1;sw33
	sw33_LM_GI.rotx = x ' VLM.Props;LM;1;sw33
	sw33_LM_flashers_l122.rotx = x ' VLM.Props;LM;1;sw33
	sw33_LM_inserts_l17.rotx = x ' VLM.Props;LM;1;sw33
	sw33_LM_inserts_l18.rotx = x ' VLM.Props;LM;1;sw33
	sw33_LM_inserts_l19.rotx = x ' VLM.Props;LM;1;sw33
	sw33_LM_flashers_l131.rotx = x ' VLM.Props;LM;1;sw33
	sw33_LM_Lit_Room.roty = y ' VLM.Props;LM;2;sw33
	sw33_LM_GI.roty = y ' VLM.Props;LM;2;sw33
	sw33_LM_flashers_l122.roty = y ' VLM.Props;LM;2;sw33
	sw33_LM_inserts_l17.roty = y ' VLM.Props;LM;2;sw33
	sw33_LM_inserts_l18.roty = y ' VLM.Props;LM;2;sw33
	sw33_LM_inserts_l19.roty = y ' VLM.Props;LM;2;sw33
	sw33_LM_flashers_l131.roty = y ' VLM.Props;LM;2;sw33

	x = sw34_BM_Dark_Room.transx
	y = sw34_BM_Dark_Room.transy
	sw34_LM_Lit_Room.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_GI.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_flashers_l122.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_inserts_l17.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_inserts_l18.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_inserts_l19.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_inserts_l20.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_inserts_l27.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_inserts_l33.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_flashers_l131.rotx = x ' VLM.Props;LM;1;sw34
	sw34_LM_Lit_Room.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_GI.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_flashers_l122.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_inserts_l17.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_inserts_l18.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_inserts_l19.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_inserts_l20.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_inserts_l27.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_inserts_l33.roty = y ' VLM.Props;LM;2;sw34
	sw34_LM_flashers_l131.roty = y ' VLM.Props;LM;2;sw34

	x = sw35_BM_Dark_Room.transx
	y = sw35_BM_Dark_Room.transy
	sw35_LM_Lit_Room.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_GI.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_inserts_l17.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_inserts_l18.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_inserts_l19.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_inserts_l20.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_inserts_l30.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_inserts_l31.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_inserts_l32.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_flashers_l131.rotx = x ' VLM.Props;LM;1;sw35
	sw35_LM_Lit_Room.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_GI.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_inserts_l17.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_inserts_l18.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_inserts_l19.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_inserts_l20.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_inserts_l30.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_inserts_l31.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_inserts_l32.roty = y ' VLM.Props;LM;2;sw35
	sw35_LM_flashers_l131.roty = y ' VLM.Props;LM;2;sw35

	x = sw36_BM_Dark_Room.transx
	y = sw36_BM_Dark_Room.transy
	sw36_LM_Lit_Room.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_GI.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_flashers_l130.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_inserts_l17.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_inserts_l18.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_inserts_l19.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_inserts_l20.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_inserts_l55.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_flashers_l131.rotx = x ' VLM.Props;LM;1;sw36
	sw36_LM_Lit_Room.roty = y ' VLM.Props;LM;2;sw36
	sw36_LM_GI.roty = y ' VLM.Props;LM;2;sw36
	sw36_LM_flashers_l130.roty = y ' VLM.Props;LM;2;sw36
	sw36_LM_inserts_l17.roty = y ' VLM.Props;LM;2;sw36
	sw36_LM_inserts_l18.roty = y ' VLM.Props;LM;2;sw36
	sw36_LM_inserts_l19.roty = y ' VLM.Props;LM;2;sw36
	sw36_LM_inserts_l20.roty = y ' VLM.Props;LM;2;sw36
	sw36_LM_inserts_l55.roty = y ' VLM.Props;LM;2;sw36
	sw36_LM_flashers_l131.roty = y ' VLM.Props;LM;2;sw36

	x = sw40_BM_Dark_Room.transx
	y = sw40_BM_Dark_Room.transy
	sw40_LM_Lit_Room.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_GI.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_flashers_l130.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_inserts_l30.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_inserts_l36.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_inserts_l37.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_inserts_l38.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_inserts_l42.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_inserts_l55.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_inserts_l63.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_inserts_l6.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_flashers_l132.rotx = x ' VLM.Props;LM;1;sw40
	sw40_LM_Lit_Room.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_GI.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_flashers_l130.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_inserts_l30.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_inserts_l36.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_inserts_l37.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_inserts_l38.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_inserts_l42.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_inserts_l55.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_inserts_l63.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_inserts_l6.roty = y ' VLM.Props;LM;2;sw40
	sw40_LM_flashers_l132.roty = y ' VLM.Props;LM;2;sw40

	x = sw41_BM_Dark_Room.transx
	y = sw41_BM_Dark_Room.transy
	sw41_LM_Lit_Room.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_GI.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_flashers_l130.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_inserts_l36.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_inserts_l37.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_inserts_l38.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_inserts_l40.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_inserts_l55.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_flashers_l132.rotx = x ' VLM.Props;LM;1;sw41
	sw41_LM_Lit_Room.roty = y ' VLM.Props;LM;2;sw41
	sw41_LM_GI.roty = y ' VLM.Props;LM;2;sw41
	sw41_LM_flashers_l130.roty = y ' VLM.Props;LM;2;sw41
	sw41_LM_inserts_l36.roty = y ' VLM.Props;LM;2;sw41
	sw41_LM_inserts_l37.roty = y ' VLM.Props;LM;2;sw41
	sw41_LM_inserts_l38.roty = y ' VLM.Props;LM;2;sw41
	sw41_LM_inserts_l40.roty = y ' VLM.Props;LM;2;sw41
	sw41_LM_inserts_l55.roty = y ' VLM.Props;LM;2;sw41
	sw41_LM_flashers_l132.roty = y ' VLM.Props;LM;2;sw41

	x = sw42_BM_Dark_Room.transx
	y = sw42_BM_Dark_Room.transy
	sw42_LM_Lit_Room.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_GI.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_flashers_l130.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_inserts_l36.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_inserts_l37.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_inserts_l38.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_inserts_l55.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_inserts_l6.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_flashers_l132.rotx = x ' VLM.Props;LM;1;sw42
	sw42_LM_Lit_Room.roty = y ' VLM.Props;LM;2;sw42
	sw42_LM_GI.roty = y ' VLM.Props;LM;2;sw42
	sw42_LM_flashers_l130.roty = y ' VLM.Props;LM;2;sw42
	sw42_LM_inserts_l36.roty = y ' VLM.Props;LM;2;sw42
	sw42_LM_inserts_l37.roty = y ' VLM.Props;LM;2;sw42
	sw42_LM_inserts_l38.roty = y ' VLM.Props;LM;2;sw42
	sw42_LM_inserts_l55.roty = y ' VLM.Props;LM;2;sw42
	sw42_LM_inserts_l6.roty = y ' VLM.Props;LM;2;sw42
	sw42_LM_flashers_l132.roty = y ' VLM.Props;LM;2;sw42
	
	x = sw44_BM_Dark_Room.transx
	y = sw44_BM_Dark_Room.transy
	sw44_LM_Lit_Room.transy=x ' VLM.Props;LM;1;sw44
	sw44_LM_GI.transy=x ' VLM.Props;LM;1;sw44
	sw44_LM_flashers_l122.transy=x ' VLM.Props;LM;1;sw44
	sw44_LM_flashers_l127.transy=x ' VLM.Props;LM;1;sw44
	sw44_LM_flashers_l130.transy=x ' VLM.Props;LM;1;sw44
	sw44_LM_inserts_l27.transy=x ' VLM.Props;LM;1;sw44
	sw44_LM_Lit_Room.transy=y ' VLM.Props;LM;2;sw44
	sw44_LM_GI.transy=y ' VLM.Props;LM;2;sw44
	sw44_LM_flashers_l122.transy=y ' VLM.Props;LM;2;sw44
	sw44_LM_flashers_l127.transy=y ' VLM.Props;LM;2;sw44
	sw44_LM_flashers_l130.transy=y ' VLM.Props;LM;2;sw44
	sw44_LM_inserts_l27.transy=y ' VLM.Props;LM;2;sw44

	x = sw45_BM_Dark_Room.transx
	y = sw45_BM_Dark_Room.transy
	sw45_LM_Lit_Room.transy=x ' VLM.Props;LM;1;sw45
	sw45_LM_GI.transy=x ' VLM.Props;LM;1;sw45
	sw45_LM_flashers_l120.transy=x ' VLM.Props;LM;1;sw45
	sw45_LM_flashers_l122.transy=x ' VLM.Props;LM;1;sw45
	sw45_LM_flashers_l129.transy=x ' VLM.Props;LM;1;sw45
	sw45_LM_flashers_l130.transy=x ' VLM.Props;LM;1;sw45
	sw45_LM_Lit_Room.transy=y ' VLM.Props;LM;2;sw45
	sw45_LM_GI.transy=y ' VLM.Props;LM;2;sw45
	sw45_LM_flashers_l120.transy=y ' VLM.Props;LM;2;sw45
	sw45_LM_flashers_l122.transy=y ' VLM.Props;LM;2;sw45
	sw45_LM_flashers_l129.transy=y ' VLM.Props;LM;2;sw45
	sw45_LM_flashers_l130.transy=y ' VLM.Props;LM;2;sw45

	x = sw46_BM_Dark_Room.transx
	y = sw46_BM_Dark_Room.transy
	sw46_LM_Lit_Room.transy=x ' VLM.Props;LM;1;sw46
	sw46_LM_GI.transy=x ' VLM.Props;LM;1;sw46
	sw46_LM_flashers_l120.transy=x ' VLM.Props;LM;1;sw46
	sw46_LM_flashers_l122.transy=x ' VLM.Props;LM;1;sw46
	sw46_LM_flashers_l123.transy=x ' VLM.Props;LM;1;sw46
	sw46_LM_flashers_l129.transy=x ' VLM.Props;LM;1;sw46
	sw46_LM_Lit_Room.transy=y ' VLM.Props;LM;2;sw46
	sw46_LM_GI.transy=y ' VLM.Props;LM;2;sw46
	sw46_LM_flashers_l120.transy=y ' VLM.Props;LM;2;sw46
	sw46_LM_flashers_l122.transy=y ' VLM.Props;LM;2;sw46
	sw46_LM_flashers_l123.transy=y ' VLM.Props;LM;2;sw46
	sw46_LM_flashers_l129.transy=y ' VLM.Props;LM;2;sw46

	x = sw50_BM_Dark_Room.transx
	y = sw50_BM_Dark_Room.transy
	sw50_LM_Lit_Room.transy=x ' VLM.Props;LM;1;sw50
	sw50_LM_GI.transy=x ' VLM.Props;LM;1;sw50
	sw50_LM_flashers_l130.transy=x ' VLM.Props;LM;1;sw50
	sw50_LM_Lit_Room.transy=y ' VLM.Props;LM;2;sw50
	sw50_LM_GI.transy=y ' VLM.Props;LM;2;sw50
	sw50_LM_flashers_l130.transy=y ' VLM.Props;LM;2;sw50

	Options_UpdateDMD

End Sub

'***********
' Table init.
'***********

Dim Mag1,Mag2,PlungerIM
Dim IMBall1, IMBall2, IMBall3, IMBall4, gBOT

Sub Table_Init
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Iron-Man Vault Edition (Stern 2014)" & vbNewLine & "VPW"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
	End With

	'************  Trough	**************
	Set IMBall4 = sw18.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set IMBall3 = sw19.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set IMBall2 = sw20.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set IMBall1 = sw21.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(IMBall1,IMBall2,IMBall3,IMBall4)
	
	Controller.Switch(18) = 1
	Controller.Switch(19) = 1
	Controller.Switch(20) = 1
	Controller.Switch(21) = 1

	Set mag1= New cvpmMagnet
	With mag1
		.InitMagnet Magnet1, 50  
		.GrabCenter = False 
		.solenoid=3
		.CreateEvents "mag1"
	End With

	Set mag2= New cvpmMagnet
	With mag2
		.InitMagnet Magnet2, 30  
		.GrabCenter = False 
		.solenoid=4
		.CreateEvents "mag2"
	End With

'Nudging
	vpmNudge.TiltSwitch=-7
	vpmNudge.Sensitivity=4
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

'Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	vpmTimer.AddTimer 1000, "WarmUpDone '"

'StandUp Init
	ResetAll
	GIOff
	Options_Load

	orbitpost False
	clanepost False

	InitVpmFFlipsSAM

End Sub

Sub WarmUpDone
	VLM_Warmup_Nestmap_0.Visible = False
	VLM_Warmup_Nestmap_1.Visible = False
	VLM_Warmup_Nestmap_2.Visible = False
	VLM_Warmup_Nestmap_3.Visible = False
	VLM_Warmup_Nestmap_4.Visible = False
	VLM_Warmup_Nestmap_5.Visible = False
	VLM_Warmup_Nestmap_6.Visible = False
	VLM_Warmup_Nestmap_7.Visible = False
	VLM_Warmup_Nestmap_8.Visible = False
	VLM_Warmup_Nestmap_9.Visible = False
	VLM_Warmup_Nestmap_10.Visible = False
	VLM_Warmup_Nestmap_11.Visible = False
	VLM_Warmup_Nestmap_12.Visible = False
	VLM_Warmup_Nestmap_13.Visible = False
	VLM_Warmup_Nestmap_14.Visible = False
End Sub

Sub Table_Paused:Controller.Pause = 1:End Sub
Sub Table_unPaused:Controller.Pause = 0:End Sub
Sub Table_exit()
	Controller.Pause = False
	Controller.Stop
End Sub

'******************************************
'	Keys
'******************************************

Sub Table_KeyDown(ByVal keycode)

	If bInOptions Then
		Options_KeyDown keycode
		Exit Sub
	End If
    If keycode = LeftMagnaSave Then
		If bOptionsMagna Then Options_Open() Else bOptionsMagna = True
    ElseIf keycode = RightMagnaSave Then
		If bOptionsMagna Then Options_Open() Else bOptionsMagna = True
	End If

	If Keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If Keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	If keycode = PlungerKey Then Plunger.Pullback : SoundPlungerPull
	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter
	If keycode = StartGameKey Then SoundStartButton
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
	If vpmKeyDown(keycode) Then Exit Sub   
End Sub
 
Sub Table_KeyUp(ByVal keycode)
	If Keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If Keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
	If Keycode = StartGameKey Then Controller.Switch(16) = 0
	If keycode = PlungerKey Then Plunger.Fire : SoundPlungerReleaseBall

    If keycode = LeftMagnaSave And Not bInOptions Then bOptionsMagna = False
    If keycode = RightMagnaSave And Not bInOptions Then bOptionsMagna = False

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

'******************************************
'	Solenoids
'******************************************

SolCallback(1) = "SolRelease"
SolCallback(2) = "solAutofire"
SolCallback(5) = "WMKick"
SolCallback(6) = "orbitpost"
SolCallback(12) = "ClanePost"
SolCallback(15) = "SolLFlipper"
SolCallback(16) = "SolRFlipper"
SolCallback(19) = "Solmonger"

SolModCallBack(20) = "SetLampMod 120,"	'Flasher Pop Bumper
SolModCallback(21) = "SetLampMod 121,"	'"FlashSol121"	'Flasher Left Ramp Top
SolModCallback(22) = "SetLampMod 122,"	'Flasher War Machine Front
SolModCallback(23) = "SetLampMod 123,"	'Flasher Monger Centre Lane
SolModCallback(24) = "SetLampMod 124,"	'Optional Coil
SolModCallback(25) = "SetLampMod 125,"	'Flasher Iron Monger x2
SolModCallback(26) = "SetLampMod 126,"	'"FlashSol126"	'Flasher Right Ramp Top
SolModCallback(27) = "SetLampMod 127,"	'Flasher War Machine x3
SolModCallback(28) = "SetLampMod 128,"	'Iron Monger Chest Light
SolModCallback(29) = "SetLampMod 129,"	'Flasher Whiplash x2
SolModCallback(30) = "SetLampMod 130,"	'Flasher Mark VI x2
SolModCallback(31) = "SetLampMod 131,"	'"FlashSol131"	'Flasher Left Ramp Bottom x2
SolModCallback(32) = "SetLampMod 132,"	'"FlashSol132"	'Flasher Right Ramp Bottom

'******************************************
'	Flipppers
'******************************************

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
		If RightFlipper.currentangle > RightFlipper.endangle - ReflipAngle Then
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

'******************************************

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
		SoundPlungerReleaseBall
	End If
End Sub

Dim ClanePostUp : ClanePostUp = False

Sub ClanePost(Enabled)	'Center Lane Drop Post
	If Enabled Then
		PostClane.Isdropped=false
		PlaySoundAtLevelStatic ("diverter_open"), PostSoundLevel, Orbitpole_Top_BM_Dark_Room
	Else
		PostClane.Isdropped=true
		PlaySoundAtLevelStatic ("diverter_close"), PostSoundLevel, Orbitpole_Top_BM_Dark_Room
	End If
	ClanePostUp = Enabled
	PostClane.TimerEnabled = True
	PostClane_Timer
End Sub

Sub PostClane_Timer
	Dim z : z = Orbitpole_Top_BM_Dark_Room.transz
	If ClanePostUp Then
		z = z - 10
		If z <= 0 Then
			z = 0
			PostClane.TimerEnabled = False
		End If
	Else
		z = z + 10
		If z >= 60 Then
			z = 60
			PostClane.TimerEnabled = False
		End If
	End If
	Orbitpole_Top_BM_Dark_Room.transz = z ' VLM.Props;BM;1;Orbitpole_Top
	Orbitpole_Top_LM_Lit_Room.transz = z ' VLM.Props;LM;1;Orbitpole_Top
	Orbitpole_Top_LM_GI.transz = z ' VLM.Props;LM;1;Orbitpole_Top
	Orbitpole_Top_LM_flashers_l120.transz = z ' VLM.Props;LM;1;Orbitpole_Top
	Orbitpole_Top_LM_flashers_l121.transz = z ' VLM.Props;LM;1;Orbitpole_Top
	Orbitpole_Top_LM_flashers_l123.transz = z ' VLM.Props;LM;1;Orbitpole_Top
	Orbitpole_Top_LM_flashers_l126.transz = z ' VLM.Props;LM;1;Orbitpole_Top
	Orbitpole_Top_LM_flashers_l129.transz = z ' VLM.Props;LM;1;Orbitpole_Top
	Orbitpole_Top_LM_inserts_l57.transz = z ' VLM.Props;LM;1;Orbitpole_Top
	Orbitpole_Top_LM_inserts_l60.transz = z ' VLM.Props;LM;1;Orbitpole_Top
End Sub

Sub PostClane_hit:RandomSoundMetal:End Sub

Dim OrbitPostUp : OrbitPostUp = False

Sub orbitpost(Enabled)	'Orbit Lane Drop Post
	If Enabled Then
		PostOrbit.Isdropped=false
		PlaySoundAtLevelStatic ("diverter_open"), PostSoundLevel, Orbitpole_01_BM_Dark_Room
	Else
		PostOrbit.Isdropped=true
		PlaySoundAtLevelStatic ("diverter_close"), PostSoundLevel, Orbitpole_01_BM_Dark_Room
	End If
	OrbitPostUp = Enabled
	PostOrbit.TimerEnabled = True
	PostOrbit_Timer
End Sub

Sub PostOrbit_Timer
	Dim z : z = Orbitpole_01_BM_Dark_Room.transz
	If OrbitPostUp Then
		z = z - 10
		If z <= 0 Then
			z = 0
			PostOrbit.TimerEnabled = False
		End If
	Else
		z = z + 10
		If z >= 60 Then
			z = 60
			PostOrbit.TimerEnabled = False
		End If
	End If
	Orbitpole_01_BM_Dark_Room.transz = z ' VLM.Props;BM;1;Orbitpole_01
	Orbitpole_01_LM_Lit_Room.transz = z ' VLM.Props;LM;1;Orbitpole_01
	Orbitpole_01_LM_GI.transz = z ' VLM.Props;LM;1;Orbitpole_01
	Orbitpole_01_LM_flashers_l120.transz = z ' VLM.Props;LM;1;Orbitpole_01
	Orbitpole_01_LM_flashers_l123.transz = z ' VLM.Props;LM;1;Orbitpole_01
	Orbitpole_01_LM_flashers_l127.transz = z ' VLM.Props;LM;1;Orbitpole_01
	Orbitpole_01_LM_flashers_l129.transz = z ' VLM.Props;LM;1;Orbitpole_01
End Sub

Sub PostOrbit_hit:RandomSoundMetal:End Sub

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

Dim KickerBall
Sub WMKick(enabled)
    If enabled Then
		If sw10.ballcntover > 0 then
			KickBall KickerBall, 165, 55, 0, 0
'			PlaySoundAt "ballhit", sw10
		End if
		controller.switch(10) = false
	End If 
End Sub

'******************************************
'    Monger Wobble
'******************************************

Const WobbleScale = 1

MongerWobbleX.interval = 34 ' Controls the speed of the wobble
Dim WobbleValueX:WobbleValueX = 0
Sub MongerWobbleX_timer
    dim imbotmove
    for each imbotmove in imbot:imbotmove.TransX = WobbleValueX:next

    if WobbleValueX < 0 then
        WobbleValueX = abs(WobbleValueX) * 0.9 - 0.1
    Else
        WobbleValueX = -abs(WobbleValueX) * 0.9 + 0.1
    end if

    if abs(WobbleValueX) < 0.1 Then
        WobbleValueX = 0
        for each imbotmove in imbot:imbotmove.TransX = WobbleValueX:next
        MongerWobbleX.Enabled = False
    end If
End Sub

MongerWobbleY.interval = 34 ' Controls the speed of the wobble
Dim WobbleValueY:WobbleValueY = 0
Sub MongerWobbleY_timer
    dim imbotmove
    for each imbotmove in imbot:imbotmove.TransY = WobbleValueY:next

    if WobbleValueY < 0 then
        WobbleValueY = abs(WobbleValueY) * 0.9 - 0.1
    Else
        WobbleValueY = -abs(WobbleValueY) * 0.9 + 0.1
    end if

    if abs(WobbleValueY) < 0.1 Then
        WobbleValueY = 0
        for each imbotmove in imbot:imbotmove.TransY = WobbleValueY:next
        MongerWobbleY.Enabled = False
    end If
End Sub

'******************************************
'    Switches
'******************************************
Dim imbotmove

'1 monger down
'3 monger up
Sub sw4_Hit
    vpmTimer.PulseSw 4
    WobbleValueX = cor.ballvelx(activeball.id)/8*WobbleScale
    MongerWobbleX.Enabled = True
End Sub

Sub sw5_Hit
    vpmTimer.PulseSw 5
    WobbleValueY = cor.ballvely(activeball.id)/8*WobbleScale
    MongerWobbleY.Enabled = True
    RandomSoundWall()
End Sub

Sub sw6_Hit
    vpmTimer.PulseSw 6
    WobbleValueX = cor.ballvelx(activeball.id)/8*WobbleScale
    MongerWobbleX.Enabled = True
End Sub

Sub sw7_Hit:Update_Wires 7, True:Controller.Switch(7) = 1:End Sub
Sub sw7_UnHit:Update_Wires 7, False:Controller.Switch(7) = 0:End Sub 
'8
Sub sw9_Hit:Update_Wires 9, True:Controller.Switch(9) = 1:End Sub
Sub sw9_UnHit:Update_Wires 9, False:Controller.Switch(9) = 0:End Sub
Sub sw10_Hit:controller.switch(10)=true:set KickerBall = activeball:End Sub
Sub sw10_unHit:controller.switch(10)=false:End Sub
Sub sw11_Spin:vpmTimer.PulseSw 11:SoundSpinner sw11:End Sub 
Sub sw12_Hit:Controller.Switch(12) = 1:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub	
Sub sw13_Spin:vpmTimer.PulseSw 13:SoundSpinner sw13:End Sub 
Sub sw14_Spin:vpmTimer.PulseSw 14:SoundSpinner sw14:End Sub 
'15	Tournament Start
'16 Start Button
'17
'18 Trough 4
'19 Trough 3
'20 Trough 2
'21 Trough 1
'22 Trough Jam
Sub sw23_Hit:Controller.Switch(23)=1:End Sub 
Sub sw23_UnHit:Controller.Switch(23)=0:End Sub 
Sub sw24_Hit:Update_Wires 24, True:Controller.Switch(24) = 1:End Sub
Sub sw24_UnHit:Update_Wires 24,False:Controller.Switch(24) = 0:End Sub
Sub sw25_Hit:Update_Wires 25, True:Controller.Switch(25) = 1:End Sub
Sub sw25_UnHit:Update_Wires 25, False:Controller.Switch(25) = 0:End Sub
'26 Slingshots
'27 Slingshots
Sub sw28_Hit:Update_Wires 28, True:Controller.Switch(28) = 1:End Sub
Sub sw28_UnHit:Update_Wires 28, False:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:Update_Wires 29, True:Controller.Switch(29) = 1:End Sub
Sub sw29_UnHit:Update_Wires 29, False:Controller.Switch(29) = 0:End Sub

Sub swplunger_Hit:Update_Wires 30, True:End Sub
Sub swplunger_UnHit:Update_Wires 30, False:End Sub
'30	Bumpers
'31	Bumpers
'32	Bumpers
'Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub 		'Left Target Bank
'Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub 		'Left Target Bank
'Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub 		'Left Target Bank
'Sub sw36_Hit:vpmTimer.PulseSw 36:End Sub		'Left Target Bank
Sub sw37_Hit:Controller.Switch(37) = 1:End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub 
Sub sw38_Hit:Update_Wires 38, True:Controller.Switch(38) = 1:End Sub
Sub sw38_UnHit:Update_Wires 38, False:Controller.Switch(38) = 0:End Sub 
Sub sw39_Hit:Update_Wires 39, True:Controller.Switch(39) = 1:End Sub
Sub sw39_UnHit:Update_Wires 39, False:Controller.Switch(39) = 0:End Sub 
'Sub sw40_Hit:vpmTimer.PulseSw 40:End Sub 		'Right Target Bank
'Sub sw41_Hit:vpmTimer.PulseSw 41:End Sub 		'Right Target Bank
'Sub sw42_Hit:vpmTimer.PulseSw 42:End Sub 		'Right Target Bank
Sub sw43_Hit:Controller.Switch(43) = 1:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
'Sub sw44_Hit:vpmTimer.PulseSw 44:End Sub		'Drone Target 1
'Sub sw45_Hit:vpmTimer.PulseSw 45:End Sub		'Drone Target 2
'Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub		'Drone Target 3
'Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub		'Whiplash Targets
'Sub sw48_Hit:vpmTimer.PulseSw 48:End Sub		'Whiplash Targets
Sub sw49_Hit:Controller.Switch(49) = 1:End Sub
Sub sw49_UnHit:Controller.Switch(49) = 0:End Sub
'Sub sw50_Hit:vpmTimer.PulseSw 50:End Sub		'Drone Target 4

Sub ResetAll()
	PostClane.Isdropped=true:PostOrbit.Isdropped=true
	switchframe.isdropped=1:sw4.isdropped=1:sw5.isdropped=1:sw6.isdropped=1
End Sub

''************************************************************************************
''*****************               Bumpers                 ****************************
''************************************************************************************

Dim bump1, bump2, bump3

Sub Bumper1_Hit
	vpmTimer.PulseSw 30
	RandomSoundBumperTop Bumper1
	bump1 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper1_Timer()
	Dim z
    Select Case bump1
        Case 1:z = 15:bump1 = 2
        Case 2:z = 25:bump1 = 3
        Case 3:z = 35:bump1 = 4
        Case 4:z = 45:Me.TimerEnabled = 0
    End Select
	Bumpers_001_BM_Dark_Room.Z = z ' VLM.Props;BM;1;Bumpers.001
	Bumpers_001_LM_Lit_Room.Z = z ' VLM.Props;LM;1;Bumpers.001
	Bumpers_001_LM_GI.Z = z ' VLM.Props;LM;1;Bumpers.001
	Bumpers_001_LM_flashers_l120.Z = z ' VLM.Props;LM;1;Bumpers.001
	Bumpers_001_LM_flashers_l123.Z = z ' VLM.Props;LM;1;Bumpers.001
	Bumpers_001_LM_flashers_l129.Z = z ' VLM.Props;LM;1;Bumpers.001
	Bumpers_001_LM_inserts_l57.Z = z ' VLM.Props;LM;1;Bumpers.001
	Bumpers_001_LM_inserts_l58.Z = z ' VLM.Props;LM;1;Bumpers.001
	Bumpers_001_LM_inserts_l9.Z = z ' VLM.Props;LM;1;Bumpers.001
End Sub

Sub Bumper2_Hit
	vpmTimer.PulseSw 31
	RandomSoundBumperMiddle Bumper2
	bump2 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper2_Timer()
	Dim z
    Select Case bump2
        Case 1:z = 15:bump2 = 2
        Case 2:z = 25:bump2 = 3
        Case 3:z = 35:bump2 = 4
        Case 4:z = 45:Me.TimerEnabled = 0
    End Select
	Bumpers_002_BM_Dark_Room.Z = z ' VLM.Props;BM;1;Bumpers.002
	Bumpers_002_LM_Lit_Room.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_GI.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_flashers_l120.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_flashers_l122.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_flashers_l123.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_flashers_l127.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_flashers_l129.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_flashers_l130.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_flashers_l121.Z = z ' VLM.Props;LM;1;Bumpers.002
	Bumpers_002_LM_flashers_l126.Z = z ' VLM.Props;LM;1;Bumpers.002
End Sub

Sub Bumper3_Hit
	vpmTimer.PulseSw 32
	RandomSoundBumperBottom Bumper3
	bump3 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper3_Timer()
	Dim z
    Select Case bump3
        Case 1:z = 15:bump3 = 2
        Case 2:z = 25:bump3 = 3
        Case 3:z = 35:bump3 = 4
        Case 4:z = 45:Me.TimerEnabled = 0
    End Select
	Bumpers_003_BM_Dark_Room.Z = z ' VLM.Props;BM;1;Bumpers.003
	Bumpers_003_LM_Lit_Room.Z = z ' VLM.Props;LM;1;Bumpers.003
	Bumpers_003_LM_GI.Z = z ' VLM.Props;LM;1;Bumpers.003
	Bumpers_003_LM_flashers_l120.Z = z ' VLM.Props;LM;1;Bumpers.003
	Bumpers_003_LM_flashers_l122.Z = z ' VLM.Props;LM;1;Bumpers.003
	Bumpers_003_LM_flashers_l129.Z = z ' VLM.Props;LM;1;Bumpers.003
	Bumpers_003_LM_flashers_l130.Z = z ' VLM.Props;LM;1;Bumpers.003
	Bumpers_003_LM_inserts_l58.Z = z ' VLM.Props;LM;1;Bumpers.003
	Bumpers_003_LM_flashers_l121.Z = z ' VLM.Props;LM;1;Bumpers.003
End Sub


'********************************************
'  Targets
'********************************************

Sub sw33_Hit:STHit 33:End Sub
Sub sw33o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw34_Hit:STHit 34:End Sub
Sub sw34o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw35_Hit:STHit 35:End Sub
Sub sw35o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw36_Hit:STHit 36:End Sub
Sub sw36o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw40_Hit:STHit 40:End Sub
Sub sw40o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw41_Hit:STHit 41:End Sub
Sub sw41o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw42_Hit:STHit 42:End Sub
Sub sw42o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw44_Hit:STHit 44:End Sub
Sub sw44o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw45_Hit:STHit 45:End Sub
Sub sw45o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw46_Hit:STHit 46:End Sub
Sub sw46o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw47_Hit:STHit 47:End Sub
Sub sw47o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw48_Hit:STHit 48:End Sub
Sub sw48o_Hit:TargetBouncer Activeball, 1:End Sub

Sub sw50_Hit:STHit 50:End Sub
Sub sw50o_Hit:TargetBouncer Activeball, 1:End Sub

'******************************************************
'						TROUGH 
'******************************************************

Sub sw18_Hit():Controller.Switch(18) = 1:UpdateTrough:End Sub
Sub sw18_UnHit():Controller.Switch(18) = 0:UpdateTrough:End Sub
Sub sw19_Hit():Controller.Switch(19) = 1:UpdateTrough:End Sub
Sub sw19_UnHit():Controller.Switch(19) = 0:UpdateTrough:End Sub
Sub sw20_Hit():Controller.Switch(20) = 1:UpdateTrough:End Sub
Sub sw20_UnHit():Controller.Switch(20) = 0:UpdateTrough:End Sub
Sub sw21_Hit():Controller.Switch(21) = 1:UpdateTrough:End Sub
Sub sw21_UnHit():Controller.Switch(21) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw21.BallCntOver = 0 Then sw20.kick 60, 9
	If sw20.BallCntOver = 0 Then sw19.kick 60, 9
	If sw19.BallCntOver = 0 Then sw18.kick 60, 9
	Me.Enabled = 0
End Sub

'******************************************************
'					DRAIN & RELEASE
'******************************************************

Sub Drain_Hit() 
	RandomSoundDrain drain
	UpdateTrough
	vpmTimer.AddTimer 500, "Drain.kick 60, 20'"
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		vpmTimer.PulseSw 15
		sw21.kick 60, 9		
		RandomSoundBallRelease sw21
	End If
End Sub

'**********************************************
' Sling Shot Animations
' Rstep and Lstep are the variables that increment the animation
'**********************************************
Dim LStep : LStep = 4 : LeftSlingShot_Timer
Dim RStep : RStep = 4 : RightSlingShot_Timer

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(Activeball)
	vpmTimer.PulseSw(27)
	RandomSoundSlingshotRight Sling_Arms_BM_Dark_Room
	RStep = -1 : RightSlingShot_Timer ' Initialize Step to 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
End Sub

Sub RightSlingShot_Timer
	Dim x1, x2, ty, tx: x1 = True:x2 = False:ty = -8:tx = 18
	Select Case RStep
		Case 3:x1 = False:x2 = True:ty = -4:tx = 7
		Case 4:x1 = False:x2 = False:ty = 0:tx = 0:RightSlingShot.TimerEnabled = 0
	End Select
	Sling_Arms_BM_Dark_Room.transx = tx ' VLM.Props;BM;1;Sling_Arms
	Sling_Arms_LM_Lit_Room.transx = tx ' VLM.Props;LM;1;Sling_Arms
	Sling_Arms_LM_GI.transx = tx ' VLM.Props;LM;1;Sling_Arms
	Sling_Arms_LM_flashers_l128.transx = tx ' VLM.Props;LM;1;Sling_Arms
	Sling_Arms_LM_flashers_l130.transx = tx ' VLM.Props;LM;1;Sling_Arms
	Sling_Arms_LM_inserts_l55.transx = tx ' VLM.Props;LM;1;Sling_Arms
	Sling_Arms_BM_Dark_Room.transy = ty ' VLM.Props;BM;2;Sling_Arms
	Sling_Arms_LM_Lit_Room.transy = ty ' VLM.Props;LM;2;Sling_Arms
	Sling_Arms_LM_GI.transy = ty ' VLM.Props;LM;2;Sling_Arms
	Sling_Arms_LM_flashers_l128.transy = ty ' VLM.Props;LM;2;Sling_Arms
	Sling_Arms_LM_flashers_l130.transy = ty ' VLM.Props;LM;2;Sling_Arms
	Sling_Arms_LM_inserts_l55.transy = ty ' VLM.Props;LM;2;Sling_Arms

	RSling1_BM_Dark_Room.Visible = x1 ' VLM.Props;BM;1;RSling1
	RSling1_LM_GI.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_flashers_l130.Visible = x1 ' VLM.Props;LM;1;RSling1

	RSling2_BM_Dark_Room.Visible = x2 ' VLM.Props;BM;1;RSling2
	RSling2_LM_GI.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_flashers_l130.Visible = x2 ' VLM.Props;LM;1;RSling2
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(Activeball)
	vpmTimer.PulseSw(26)
	RandomSoundSlingshotLeft Sling_Arms_001_BM_Dark_Room
	LStep = -1 : LeftSlingShot_Timer ' Initialize Step to 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
End Sub

Sub LeftSlingShot_Timer
	Dim x1, x2, ty, tx: x1 = True:x2 = False:ty = -11:tx = -24
    Select Case LStep
        Case 3:x1 = False:x2 = True:ty = -7:tx = -12
        Case 4:x1 = False:x2 = False:ty = 0:tx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
	Sling_Arms_001_BM_Dark_Room.transx = tx ' VLM.Props;BM;1;Sling_Arms.001
	Sling_Arms_001_LM_Lit_Room.transx = tx ' VLM.Props;LM;1;Sling_Arms.001
	Sling_Arms_001_LM_GI.transx = tx ' VLM.Props;LM;1;Sling_Arms.001
	Sling_Arms_001_LM_flashers_l128.transx = tx ' VLM.Props;LM;1;Sling_Arms.001
	Sling_Arms_001_LM_flashers_l130.transx = tx ' VLM.Props;LM;1;Sling_Arms.001
	Sling_Arms_001_LM_inserts_l55.transx = tx ' VLM.Props;LM;1;Sling_Arms.001
	Sling_Arms_001_BM_Dark_Room.transy = ty ' VLM.Props;BM;2;Sling_Arms.001
	Sling_Arms_001_LM_Lit_Room.transy = ty ' VLM.Props;LM;2;Sling_Arms.001
	Sling_Arms_001_LM_GI.transy = ty ' VLM.Props;LM;2;Sling_Arms.001
	Sling_Arms_001_LM_flashers_l128.transy = ty ' VLM.Props;LM;2;Sling_Arms.001
	Sling_Arms_001_LM_flashers_l130.transy = ty ' VLM.Props;LM;2;Sling_Arms.001
	Sling_Arms_001_LM_inserts_l55.transy = ty ' VLM.Props;LM;2;Sling_Arms.001

	LSling1_BM_Dark_Room.Visible = x1 ' VLM.Props;BM;1;LSling1
	LSling1_LM_GI.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_flashers_l130.Visible = x1 ' VLM.Props;LM;1;LSling1

	LSling2_BM_Dark_Room.Visible = x2 ' VLM.Props;BM;1;LSling2
	LSling2_LM_GI.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_flashers_l130.Visible = x2 ' VLM.Props;LM;1;LSling2
    LStep = LStep + 1
End Sub

'Impulse Plunger
Const IMPowerSetting = 50
Const IMTime = 0.6
Set plungerIM = New cvpmImpulseP
With plungerIM
    .InitImpulseP swplunger, IMPowerSetting, IMTime
    .Random 0.3
    '.InitExitSnd "plunger2", "plunger"
    .CreateEvents "plungerIM"
End With

'******************************************************
' ZLAM  LAMPZ by nFozzy
'
' 2021.07.01 Added modulated flashers
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments
LampTimer.Interval = -1		' Using fixed value so the fading speed is same for every fps
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update2	'update (fading logic only)
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub InitLampsNF()
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (max level / full MS fading time)
	Dim x
	for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/120 : next

	' FLashers
	for x = 120 to 132 : Lampz.FadeSpeedUp(x) = 255/40 : Lampz.FadeSpeedDown(x) = 255/400 : Lampz.Modulate(x) = 1.0/255 : next

	' Hide all lights used for ball reflection only (only hide the direct halo and the transmission part, not the reflection on balls)
'	For each x in BallReflections : x.visible = False : Next

	' Room lighting
	Lampz.FadeSpeedUp(150) = 100/1 : Lampz.FadeSpeedDown(150) = 100/1 : Lampz.Modulate(150) = 1/100
	
	' lamps ending with a 'r' (like l10r) are for ball reflection only
	' lamps ending with a 'a' (like l10a) are for giveing some trnalsucency to insert overlay
	

	' Lampz.MassAssign(150) = L150 ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Layer1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Layer2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Parts_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Bulb_left_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Bulb_right_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Bumpers_001_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Bumpers_002_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Bumpers_003_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Gate01_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Gate_SpinLeft_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Gate_SpinMid_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Gate_SpinRight_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap LFLogo_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Orbitpole_01_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Orbitpole_Top_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RFLogo_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Sling_Arms_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Sling_Arms_001_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap gate02_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap gate03_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap loutpost_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap routpost_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw24_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw25_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw28_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw29_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw33_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw34_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw35_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw36_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw38_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw39_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw40_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw41_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw42_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw44_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw45_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw46_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw47_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw48_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw50_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw7_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw9_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap swplunger_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap IMTop_LM_Lit_Room, IMOpacity, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap IMTopUpped_LM_Lit_Room, 100.0 - IMOpacity, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap IMBot_LM_Lit_Room, 100.0-0.5*IMOpacity, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Parts_LM_Lit_Room_001, 100.0, " ' VLM.Lampz;Lit Room

	' Lampz.MassAssign(104) = l104 ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Layer1_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Layer2_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Parts_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Bulb_left_LM_GI, 200.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Bulb_right_LM_GI, 400.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Bumpers_001_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Bumpers_002_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Bumpers_003_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Gate01_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Gate_SpinLeft_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Gate_SpinMid_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Gate_SpinRight_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap LFLogo_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap LSling1_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap LSling2_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Orbitpole_01_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Orbitpole_Top_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap RFLogo_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap RSling1_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap RSling2_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Sling_Arms_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Sling_Arms_001_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap gate02_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap gate03_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap gate04_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap gate05_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap loutpost_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap routpost_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw24_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw25_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw28_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw29_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw33_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw34_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw35_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw36_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw38_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw39_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw40_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw41_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw42_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw44_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw45_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw46_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw47_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw48_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw50_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw7_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap sw9_LM_GI, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap IMTop_LM_GI, IMOpacity, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap IMTopUpped_LM_GI, 100.0 - IMOpacity, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap IMBot_LM_GI, 100.0-0.5*IMOpacity, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Parts_LM_GI_001, 100.0, " ' VLM.Lampz;GI
	Lampz.Callback(104) = "UpdateLightMap Parts_LM_GI_002, 100.0, " ' VLM.Lampz;GI
	Lampz.MassAssign(104)= ColToArray(GILights)
	Lampz.Callback(104) = "FadeMaterialColor PlayfieldInsertOutline, 255, 30, "	'fading for insert text ramp

	'Flasher Pop Bumper (Playfield Insert)
	Lampz.MassAssign(120) = l120 ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Layer1_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Layer2_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Parts_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Bumpers_001_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Bumpers_002_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Bumpers_003_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Gate_SpinMid_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Orbitpole_01_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap Orbitpole_Top_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap gate03_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap gate04_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap gate05_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap sw38_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap sw39_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap sw45_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap sw46_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap sw9_LM_flashers_l120, 100.0, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap IMTop_LM_flashers_l120, IMOpacity, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap IMTopUpped_LM_flashers_l120, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l120
	Lampz.Callback(120) = "UpdateLightMap IMBot_LM_flashers_l120, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l120
	Lampz.MassAssign(120) = l120
	Lampz.Callback(120) = "DisableLighting p120, 350,"

	Lampz.MassAssign(121) = l121 ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap Layer1_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap Layer2_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap Parts_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap Bumpers_002_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap Bumpers_003_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap Gate_SpinMid_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap Orbitpole_Top_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap gate02_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap sw39_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap sw7_LM_flashers_l121, 100.0, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap IMTop_LM_flashers_l121, IMOpacity, " ' VLM.Lampz;flashers-l121
	Lampz.Callback(121) = "UpdateLightMap IMBot_LM_flashers_l121, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l121

	'Flasher War Machine Front (Playfield Insert)
	Lampz.MassAssign(122) = l122 ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap Layer1_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap Parts_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap Bumpers_002_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap Bumpers_003_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap Gate_SpinLeft_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap Gate_SpinMid_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap Gate_SpinRight_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap sw33_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap sw34_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap sw44_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap sw45_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap sw46_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap sw47_LM_flashers_l122, 100.0, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap IMTop_LM_flashers_l122, IMOpacity, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap IMTopUpped_LM_flashers_l122, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l122
	Lampz.Callback(122) = "UpdateLightMap IMBot_LM_flashers_l122, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l122
'	Lampz.Callback(122) = "DisableLighting p21, 550,"

	'Flasher Monger Centre Lane (Playfield Insert)
	Lampz.MassAssign(123) = l123 ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap Layer1_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap Layer2_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap Parts_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap Bumpers_001_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap Bumpers_002_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap Gate_SpinMid_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap Orbitpole_01_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap Orbitpole_Top_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap gate05_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap sw46_LM_flashers_l123, 100.0, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap IMTop_LM_flashers_l123, IMOpacity, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap IMTopUpped_LM_flashers_l123, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l123
	Lampz.Callback(123) = "UpdateLightMap IMBot_LM_flashers_l123, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l123
	' Lampz.Callback(123) = "DisableLighting p10, 350,"

	Lampz.MassAssign(125) = l125 ' VLM.Lampz;flashers-l125
	Lampz.Callback(125) = "UpdateLightMap Parts_LM_flashers_l125, 100.0, " ' VLM.Lampz;flashers-l125
	Lampz.Callback(125) = "UpdateLightMap Gate_SpinLeft_LM_flashers_l125, 100.0, " ' VLM.Lampz;flashers-l125
	Lampz.Callback(125) = "UpdateLightMap IMTopUpped_LM_flashers_l125, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l125
	Lampz.Callback(125) = "UpdateLightMap IMBot_LM_flashers_l125, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l125

	Lampz.MassAssign(126) = l126 ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap Layer1_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap Layer2_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap Parts_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap Bumpers_002_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap Gate_SpinMid_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap Orbitpole_Top_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap gate03_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap gate04_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap sw25_LM_flashers_l126, 100.0, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap IMTop_LM_flashers_l126, IMOpacity, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap IMTopUpped_LM_flashers_l126, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l126
	Lampz.Callback(126) = "UpdateLightMap IMBot_LM_flashers_l126, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l126

	'Flasher War Machine Kickback (Playfield Insert)
	' Lampz.MassAssign(127) = l127 ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap Layer1_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap Parts_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap Bulb_left_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap Bumpers_002_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap Gate01_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap Gate_SpinLeft_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap Gate_SpinMid_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap Orbitpole_01_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap sw44_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap sw7_LM_flashers_l127, 100.0, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap IMTop_LM_flashers_l127, IMOpacity, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap IMTopUpped_LM_flashers_l127, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l127
	Lampz.Callback(127) = "UpdateLightMap IMBot_LM_flashers_l127, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l127
	Lampz.MassAssign(127) = l127a
	Lampz.MassAssign(127) = l127b
	Lampz.Callback(127) = "DisableLighting p127, 350,"

	' Lampz.MassAssign(128) = l128 ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap Layer1_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap Parts_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap Bulb_left_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap Bulb_right_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap Sling_Arms_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap Sling_Arms_001_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap routpost_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap sw25_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap sw28_LM_flashers_l128, 100.0, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap IMTopUpped_LM_flashers_l128, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l128
	Lampz.Callback(128) = "UpdateLightMap IMBot_LM_flashers_l128, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l128

	' Lampz.MassAssign(129) = l129 ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Layer1_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Layer2_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Parts_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Bumpers_001_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Bumpers_002_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Bumpers_003_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Gate_SpinLeft_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Gate_SpinMid_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Gate_SpinRight_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Orbitpole_01_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap Orbitpole_Top_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap gate03_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap sw25_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap sw38_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap sw39_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap sw45_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap sw46_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap sw47_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap sw48_LM_flashers_l129, 100.0, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap IMTop_LM_flashers_l129, IMOpacity, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap IMTopUpped_LM_flashers_l129, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l129
	Lampz.Callback(129) = "UpdateLightMap IMBot_LM_flashers_l129, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l129

	Lampz.MassAssign(130) = l130 ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap Layer1_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap Parts_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap Bulb_left_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap Bulb_right_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap Bumpers_002_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap Bumpers_003_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap LFLogo_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap LSling1_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap LSling2_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap RFLogo_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap RSling1_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap RSling2_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap Sling_Arms_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap Sling_Arms_001_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap gate03_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap loutpost_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap routpost_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw24_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw25_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw28_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw29_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw36_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw40_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw41_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw42_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw44_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw45_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw47_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw48_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap sw50_LM_flashers_l130, 100.0, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap IMTopUpped_LM_flashers_l130, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l130
	Lampz.Callback(130) = "UpdateLightMap IMBot_LM_flashers_l130, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l130

	Lampz.MassAssign(131) = l131 ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap Layer1_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap Layer2_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap Parts_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap Bulb_left_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap Bulb_right_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap Gate01_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap Gate_SpinLeft_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap loutpost_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap routpost_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap sw24_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap sw33_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap sw34_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap sw35_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap sw36_LM_flashers_l131, 100.0, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap IMTop_LM_flashers_l131, IMOpacity, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap IMTopUpped_LM_flashers_l131, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l131
	Lampz.Callback(131) = "UpdateLightMap IMBot_LM_flashers_l131, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l131

	Lampz.MassAssign(132) = l132 ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap Layer1_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap Parts_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap Bulb_left_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap Bulb_right_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap Gate_SpinRight_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap routpost_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap sw29_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap sw40_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap sw41_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap sw42_LM_flashers_l132, 100.0, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap IMTop_LM_flashers_l132, IMOpacity, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap IMTopUpped_LM_flashers_l132, 100.0 - IMOpacity, " ' VLM.Lampz;flashers-l132
	Lampz.Callback(132) = "UpdateLightMap IMBot_LM_flashers_l132, 100.0-0.5*IMOpacity, " ' VLM.Lampz;flashers-l132

	' Lampz.MassAssign(1) = l1	'Start Button
	Lampz.Callback(1) = "DisableLighting VR_StartButtInner, 30,"

'	Lampz.MassAssign(2) = l2	'Tournament Button
	Lampz.Callback(2) = "DisableLighting VR_TourneyButt, 30,"

	Lampz.MassAssign(3) = l3 ' VLM.Lampz;inserts-l3
	Lampz.Callback(3) = "UpdateLightMap Parts_LM_inserts_l3, 100.0, " ' VLM.Lampz;inserts-l3
	Lampz.Callback(3) = "UpdateLightMap LFLogo_LM_inserts_l3, 100.0, " ' VLM.Lampz;inserts-l3
	Lampz.Callback(3) = "UpdateLightMap RFLogo_LM_inserts_l3, 100.0, " ' VLM.Lampz;inserts-l3
	Lampz.Callback(3) = "DisableLighting p3, 50,"
	Lampz.MassAssign(3) = l3 ' VLM.Lampz;inserts-l3
	Lampz.Callback(3) = "UpdateLightMap Parts_LM_inserts_l3, 100.0, " ' VLM.Lampz;inserts-l3
	Lampz.Callback(3) = "UpdateLightMap LFLogo_LM_inserts_l3, 100.0, " ' VLM.Lampz;inserts-l3
	Lampz.Callback(3) = "UpdateLightMap RFLogo_LM_inserts_l3, 100.0, " ' VLM.Lampz;inserts-l3

	Lampz.MassAssign(4) = l4 ' VLM.Lampz;inserts-l4
	Lampz.Callback(4) = "UpdateLightMap Layer1_LM_inserts_l4, 100.0, " ' VLM.Lampz;inserts-l4
	Lampz.Callback(4) = "UpdateLightMap Parts_LM_inserts_l4, 100.0, " ' VLM.Lampz;inserts-l4
'	Lampz.Callback(4) = "DisableLighting p4, 50,"

	Lampz.MassAssign(5) = l5 ' VLM.Lampz;inserts-l5
	Lampz.Callback(5) = "UpdateLightMap Layer1_LM_inserts_l5, 100.0, " ' VLM.Lampz;inserts-l5
	Lampz.Callback(5) = "UpdateLightMap Parts_LM_inserts_l5, 100.0, " ' VLM.Lampz;inserts-l5
'	Lampz.Callback(5) = "DisableLighting p5, 50,"

	Lampz.MassAssign(6) = l6 ' VLM.Lampz;inserts-l6
	Lampz.Callback(6) = "UpdateLightMap Layer1_LM_inserts_l6, 100.0, " ' VLM.Lampz;inserts-l6
	Lampz.Callback(6) = "UpdateLightMap Parts_LM_inserts_l6, 100.0, " ' VLM.Lampz;inserts-l6
	Lampz.Callback(6) = "UpdateLightMap sw40_LM_inserts_l6, 100.0, " ' VLM.Lampz;inserts-l6
	Lampz.Callback(6) = "UpdateLightMap sw42_LM_inserts_l6, 100.0, " ' VLM.Lampz;inserts-l6
'	Lampz.Callback(6) = "DisableLighting p6, 50,"

	Lampz.MassAssign(7) = l7 ' VLM.Lampz;inserts-l7
	Lampz.Callback(7) = "UpdateLightMap Layer1_LM_inserts_l7, 100.0, " ' VLM.Lampz;inserts-l7
	Lampz.Callback(7) = "UpdateLightMap Parts_LM_inserts_l7, 100.0, " ' VLM.Lampz;inserts-l7
'	Lampz.Callback(7) = "DisableLighting p7, 50,"

	Lampz.MassAssign(8) = L8 ' VLM.Lampz;inserts-L8
	Lampz.Callback(8) = "UpdateLightMap Layer1_LM_inserts_L8, 100.0, " ' VLM.Lampz;inserts-L8
	Lampz.Callback(8) = "UpdateLightMap Parts_LM_inserts_L8, 100.0, " ' VLM.Lampz;inserts-L8
	Lampz.Callback(8) = "DisableLighting p8, 50,"
	Lampz.MassAssign(8) = L8 ' VLM.Lampz;inserts-L8
	Lampz.Callback(8) = "UpdateLightMap Layer1_LM_inserts_L8, 100.0, " ' VLM.Lampz;inserts-L8
	Lampz.Callback(8) = "UpdateLightMap Parts_LM_inserts_L8, 100.0, " ' VLM.Lampz;inserts-L8

	Lampz.MassAssign(9) = l9 ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "UpdateLightMap Parts_LM_inserts_l9, 100.0, " ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "UpdateLightMap Bumpers_001_LM_inserts_l9, 100.0, " ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "UpdateLightMap Gate_SpinMid_LM_inserts_l9, 100.0, " ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "UpdateLightMap gate05_LM_inserts_l9, 100.0, " ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "DisableLighting p9, 50,"
	Lampz.MassAssign(9) = l9 ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "UpdateLightMap Parts_LM_inserts_l9, 100.0, " ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "UpdateLightMap Bumpers_001_LM_inserts_l9, 100.0, " ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "UpdateLightMap Gate_SpinMid_LM_inserts_l9, 100.0, " ' VLM.Lampz;inserts-l9
	Lampz.Callback(9) = "UpdateLightMap gate05_LM_inserts_l9, 100.0, " ' VLM.Lampz;inserts-l9

	Lampz.MassAssign(10) = l10 ' VLM.Lampz;inserts-l10
	Lampz.Callback(10) = "UpdateLightMap Parts_LM_inserts_l10, 100.0, " ' VLM.Lampz;inserts-l10
	Lampz.Callback(10) = "UpdateLightMap Gate_SpinMid_LM_inserts_l10, 100.0, " ' VLM.Lampz;inserts-l10
	Lampz.Callback(10) = "DisableLighting p10, 50,"

	Lampz.MassAssign(11) = l11 ' VLM.Lampz;inserts-l11
	Lampz.Callback(11) = "UpdateLightMap Parts_LM_inserts_l11, 100.0, " ' VLM.Lampz;inserts-l11
	Lampz.Callback(11) = "DisableLighting p11, 50,"

	Lampz.MassAssign(12) = l12 ' VLM.Lampz;inserts-l12
	Lampz.Callback(12) = "UpdateLightMap Layer2_LM_inserts_l12, 100.0, " ' VLM.Lampz;inserts-l12
	Lampz.Callback(12) = "UpdateLightMap Parts_LM_inserts_l12, 100.0, " ' VLM.Lampz;inserts-l12
	Lampz.Callback(12) = "DisableLighting p12, 150,"

	Lampz.MassAssign(13) = l13 ' VLM.Lampz;inserts-l13
	Lampz.Callback(13) = "UpdateLightMap IMTopUpped_LM_inserts_l13, 100.0 - IMOpacity, " ' VLM.Lampz;inserts-l13
	Lampz.Callback(13) = "DisableLighting p13, 150,"

	Lampz.MassAssign(14) = l14 ' VLM.Lampz;inserts-l14
	Lampz.Callback(14) = "UpdateLightMap Layer1_LM_inserts_l14, 100.0, " ' VLM.Lampz;inserts-l14
	Lampz.Callback(14) = "UpdateLightMap Parts_LM_inserts_l14, 100.0, " ' VLM.Lampz;inserts-l14
	Lampz.Callback(14) = "DisableLighting p14, 50,"

	Lampz.MassAssign(15) = l15
	Lampz.Callback(15) = "DisableLighting p15, 50,"

	Lampz.MassAssign(16) = l16 ' VLM.Lampz;inserts-l16
	Lampz.Callback(16) = "UpdateLightMap Layer1_LM_inserts_l16, 100.0, " ' VLM.Lampz;inserts-l16
	Lampz.Callback(16) = "UpdateLightMap Parts_LM_inserts_l16, 100.0, " ' VLM.Lampz;inserts-l16
	Lampz.Callback(16) = "UpdateLightMap Gate_SpinLeft_LM_inserts_l16, 100.0, " ' VLM.Lampz;inserts-l16
	Lampz.Callback(16) = "DisableLighting p16, 50,"

	Lampz.MassAssign(17) = l17 ' VLM.Lampz;inserts-l17
	Lampz.Callback(17) = "UpdateLightMap Layer1_LM_inserts_l17, 100.0, " ' VLM.Lampz;inserts-l17
	Lampz.Callback(17) = "UpdateLightMap Parts_LM_inserts_l17, 100.0, " ' VLM.Lampz;inserts-l17
	Lampz.Callback(17) = "UpdateLightMap Gate_SpinLeft_LM_inserts_l17, 100.0, " ' VLM.Lampz;inserts-l17
	Lampz.Callback(17) = "UpdateLightMap sw33_LM_inserts_l17, 100.0, " ' VLM.Lampz;inserts-l17
	Lampz.Callback(17) = "UpdateLightMap sw34_LM_inserts_l17, 100.0, " ' VLM.Lampz;inserts-l17
	Lampz.Callback(17) = "UpdateLightMap sw35_LM_inserts_l17, 100.0, " ' VLM.Lampz;inserts-l17
	Lampz.Callback(17) = "UpdateLightMap sw36_LM_inserts_l17, 100.0, " ' VLM.Lampz;inserts-l17
	Lampz.Callback(17) = "DisableLighting p17, 250,"

	Lampz.MassAssign(18) = l18 ' VLM.Lampz;inserts-l18
	Lampz.Callback(18) = "UpdateLightMap Layer1_LM_inserts_l18, 100.0, " ' VLM.Lampz;inserts-l18
	Lampz.Callback(18) = "UpdateLightMap sw33_LM_inserts_l18, 100.0, " ' VLM.Lampz;inserts-l18
	Lampz.Callback(18) = "UpdateLightMap sw34_LM_inserts_l18, 100.0, " ' VLM.Lampz;inserts-l18
	Lampz.Callback(18) = "UpdateLightMap sw35_LM_inserts_l18, 100.0, " ' VLM.Lampz;inserts-l18
	Lampz.Callback(18) = "UpdateLightMap sw36_LM_inserts_l18, 100.0, " ' VLM.Lampz;inserts-l18
	Lampz.Callback(18) = "DisableLighting p18, 250,"

	Lampz.MassAssign(19) = l19 ' VLM.Lampz;inserts-l19
	Lampz.Callback(19) = "UpdateLightMap Layer1_LM_inserts_l19, 100.0, " ' VLM.Lampz;inserts-l19
	Lampz.Callback(19) = "UpdateLightMap sw33_LM_inserts_l19, 100.0, " ' VLM.Lampz;inserts-l19
	Lampz.Callback(19) = "UpdateLightMap sw34_LM_inserts_l19, 100.0, " ' VLM.Lampz;inserts-l19
	Lampz.Callback(19) = "UpdateLightMap sw35_LM_inserts_l19, 100.0, " ' VLM.Lampz;inserts-l19
	Lampz.Callback(19) = "UpdateLightMap sw36_LM_inserts_l19, 100.0, " ' VLM.Lampz;inserts-l19
	Lampz.Callback(19) = "DisableLighting p19, 250,"

	Lampz.MassAssign(20) = l20 ' VLM.Lampz;inserts-l20
	Lampz.Callback(20) = "UpdateLightMap Layer1_LM_inserts_l20, 100.0, " ' VLM.Lampz;inserts-l20
	Lampz.Callback(20) = "UpdateLightMap Parts_LM_inserts_l20, 100.0, " ' VLM.Lampz;inserts-l20
	Lampz.Callback(20) = "UpdateLightMap sw34_LM_inserts_l20, 100.0, " ' VLM.Lampz;inserts-l20
	Lampz.Callback(20) = "UpdateLightMap sw35_LM_inserts_l20, 100.0, " ' VLM.Lampz;inserts-l20
	Lampz.Callback(20) = "UpdateLightMap sw36_LM_inserts_l20, 100.0, " ' VLM.Lampz;inserts-l20
	Lampz.Callback(20) = "DisableLighting p20, 250,"

	Lampz.MassAssign(21) = l21 ' VLM.Lampz;inserts-l21
	Lampz.Callback(21) = "UpdateLightMap Parts_LM_inserts_l21, 100.0, " ' VLM.Lampz;inserts-l21
	Lampz.Callback(21) = "DisableLighting p21, 150,"

	Lampz.MassAssign(22) = l22 ' VLM.Lampz;inserts-l22
	Lampz.Callback(22) = "UpdateLightMap Parts_LM_inserts_l22, 100.0, " ' VLM.Lampz;inserts-l22
	Lampz.Callback(22) = "UpdateLightMap IMTopUpped_LM_inserts_l22, 100.0 - IMOpacity, " ' VLM.Lampz;inserts-l22
	Lampz.Callback(22) = "DisableLighting p22, 50,"

	Lampz.MassAssign(23) = l23 ' VLM.Lampz;inserts-l23
	Lampz.Callback(23) = "UpdateLightMap Parts_LM_inserts_l23, 100.0, " ' VLM.Lampz;inserts-l23
	Lampz.Callback(23) = "UpdateLightMap IMTopUpped_LM_inserts_l23, 100.0 - IMOpacity, " ' VLM.Lampz;inserts-l23
	Lampz.Callback(23) = "UpdateLightMap IMBot_LM_inserts_l23, 100.0-0.5*IMOpacity, " ' VLM.Lampz;inserts-l23
	Lampz.Callback(23) = "DisableLighting p23, 50,"

	Lampz.MassAssign(24) = l24 ' VLM.Lampz;inserts-l24
	Lampz.Callback(24) = "UpdateLightMap Parts_LM_inserts_l24, 100.0, " ' VLM.Lampz;inserts-l24
	Lampz.Callback(24) = "UpdateLightMap IMTopUpped_LM_inserts_l24, 100.0 - IMOpacity, " ' VLM.Lampz;inserts-l24
	Lampz.Callback(24) = "UpdateLightMap IMBot_LM_inserts_l24, 100.0-0.5*IMOpacity, " ' VLM.Lampz;inserts-l24
	Lampz.Callback(24) = "DisableLighting p24, 50,"

	Lampz.MassAssign(25) = l25 ' VLM.Lampz;inserts-l25
	Lampz.Callback(25) = "UpdateLightMap Parts_LM_inserts_l25, 100.0, " ' VLM.Lampz;inserts-l25
	Lampz.Callback(25) = "UpdateLightMap IMTopUpped_LM_inserts_l25, 100.0 - IMOpacity, " ' VLM.Lampz;inserts-l25
	Lampz.Callback(25) = "DisableLighting p25, 50,"

	Lampz.MassAssign(26) = l26 ' VLM.Lampz;inserts-l26
	Lampz.Callback(26) = "UpdateLightMap Layer1_LM_inserts_l26, 100.0, " ' VLM.Lampz;inserts-l26
	Lampz.Callback(26) = "UpdateLightMap Layer2_LM_inserts_l26, 100.0, " ' VLM.Lampz;inserts-l26
	Lampz.Callback(26) = "UpdateLightMap Parts_LM_inserts_l26, 100.0, " ' VLM.Lampz;inserts-l26
	Lampz.Callback(26) = "DisableLighting p26, 50,"

	Lampz.MassAssign(27) = l27 ' VLM.Lampz;inserts-l27
	Lampz.Callback(27) = "UpdateLightMap Parts_LM_inserts_l27, 100.0, " ' VLM.Lampz;inserts-l27
	Lampz.Callback(27) = "UpdateLightMap Gate_SpinLeft_LM_inserts_l27, 100.0, " ' VLM.Lampz;inserts-l27
	Lampz.Callback(27) = "UpdateLightMap sw34_LM_inserts_l27, 100.0, " ' VLM.Lampz;inserts-l27
	Lampz.Callback(27) = "UpdateLightMap sw44_LM_inserts_l27, 100.0, " ' VLM.Lampz;inserts-l27
	Lampz.Callback(27) = "UpdateLightMap IMTopUpped_LM_inserts_l27, 100.0 - IMOpacity, " ' VLM.Lampz;inserts-l27
	Lampz.Callback(27) = "UpdateLightMap IMBot_LM_inserts_l27, 100.0-0.5*IMOpacity, " ' VLM.Lampz;inserts-l27

	Lampz.MassAssign(28) = l28
	Lampz.Callback(28) = "DisableLighting p28, 50,"

	Lampz.MassAssign(29) = l29
	Lampz.Callback(29) = "DisableLighting p29, 50,"

	Lampz.MassAssign(30) = l30 ' VLM.Lampz;inserts-l30
	Lampz.Callback(30) = "UpdateLightMap sw35_LM_inserts_l30, 100.0, " ' VLM.Lampz;inserts-l30
	Lampz.Callback(30) = "UpdateLightMap sw40_LM_inserts_l30, 100.0, " ' VLM.Lampz;inserts-l30
	Lampz.Callback(30) = "DisableLighting p30, 50,"

	Lampz.MassAssign(31) = l31 ' VLM.Lampz;inserts-l31
	Lampz.Callback(31) = "UpdateLightMap sw35_LM_inserts_l31, 100.0, " ' VLM.Lampz;inserts-l31
	Lampz.Callback(31) = "DisableLighting p31, 50,"

	Lampz.MassAssign(32) = l32 ' VLM.Lampz;inserts-l32
	Lampz.Callback(32) = "UpdateLightMap sw35_LM_inserts_l32, 100.0, " ' VLM.Lampz;inserts-l32
	Lampz.Callback(32) = "DisableLighting p32, 50,"

	Lampz.MassAssign(33) = l33 ' VLM.Lampz;inserts-l33
	Lampz.Callback(33) = "UpdateLightMap sw34_LM_inserts_l33, 100.0, " ' VLM.Lampz;inserts-l33
	Lampz.Callback(33) = "DisableLighting p33, 50,"

	Lampz.MassAssign(34) = l34 ' VLM.Lampz;inserts-l34
	Lampz.Callback(34) = "UpdateLightMap Layer1_LM_inserts_l34, 100.0, " ' VLM.Lampz;inserts-l34
	Lampz.Callback(34) = "UpdateLightMap Layer2_LM_inserts_l34, 100.0, " ' VLM.Lampz;inserts-l34
	Lampz.Callback(34) = "UpdateLightMap Parts_LM_inserts_l34, 100.0, " ' VLM.Lampz;inserts-l34
	Lampz.Callback(34) = "DisableLighting p34, 50,"

	Lampz.MassAssign(35) = l35 ' VLM.Lampz;inserts-l35
	Lampz.Callback(35) = "UpdateLightMap Layer1_LM_inserts_l35, 100.0, " ' VLM.Lampz;inserts-l35
	Lampz.Callback(35) = "UpdateLightMap Gate_SpinRight_LM_inserts_l35, 100.0, " ' VLM.Lampz;inserts-l35
	Lampz.Callback(35) = "DisableLighting p35, 50,"

	Lampz.MassAssign(36) = l36 ' VLM.Lampz;inserts-l36
	Lampz.Callback(36) = "UpdateLightMap Layer1_LM_inserts_l36, 100.0, " ' VLM.Lampz;inserts-l36
	Lampz.Callback(36) = "UpdateLightMap Parts_LM_inserts_l36, 100.0, " ' VLM.Lampz;inserts-l36
	Lampz.Callback(36) = "UpdateLightMap sw40_LM_inserts_l36, 100.0, " ' VLM.Lampz;inserts-l36
	Lampz.Callback(36) = "UpdateLightMap sw41_LM_inserts_l36, 100.0, " ' VLM.Lampz;inserts-l36
	Lampz.Callback(36) = "UpdateLightMap sw42_LM_inserts_l36, 100.0, " ' VLM.Lampz;inserts-l36
	Lampz.Callback(36) = "DisableLighting p36, 250,"

	Lampz.MassAssign(37) = l37 ' VLM.Lampz;inserts-l37
	Lampz.Callback(37) = "UpdateLightMap Layer1_LM_inserts_l37, 100.0, " ' VLM.Lampz;inserts-l37
	Lampz.Callback(37) = "UpdateLightMap Parts_LM_inserts_l37, 100.0, " ' VLM.Lampz;inserts-l37
	Lampz.Callback(37) = "UpdateLightMap Gate_SpinRight_LM_inserts_l37, 100.0, " ' VLM.Lampz;inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw40_LM_inserts_l37, 100.0, " ' VLM.Lampz;inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw41_LM_inserts_l37, 100.0, " ' VLM.Lampz;inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw42_LM_inserts_l37, 100.0, " ' VLM.Lampz;inserts-l37
	Lampz.Callback(37) = "DisableLighting p37, 250,"

	Lampz.MassAssign(38) = l38 ' VLM.Lampz;inserts-l38
	Lampz.Callback(38) = "UpdateLightMap Layer1_LM_inserts_l38, 100.0, " ' VLM.Lampz;inserts-l38
	Lampz.Callback(38) = "UpdateLightMap Parts_LM_inserts_l38, 100.0, " ' VLM.Lampz;inserts-l38
	Lampz.Callback(38) = "UpdateLightMap sw40_LM_inserts_l38, 100.0, " ' VLM.Lampz;inserts-l38
	Lampz.Callback(38) = "UpdateLightMap sw41_LM_inserts_l38, 100.0, " ' VLM.Lampz;inserts-l38
	Lampz.Callback(38) = "UpdateLightMap sw42_LM_inserts_l38, 100.0, " ' VLM.Lampz;inserts-l38
	Lampz.Callback(38) = "DisableLighting p38, 250,"

	Lampz.MassAssign(39) = l39 ' VLM.Lampz;inserts-l39
	Lampz.Callback(39) = "UpdateLightMap Parts_LM_inserts_l39, 100.0, " ' VLM.Lampz;inserts-l39
	Lampz.Callback(39) = "UpdateLightMap Gate_SpinRight_LM_inserts_l39, 100.0, " ' VLM.Lampz;inserts-l39
	Lampz.Callback(39) = "DisableLighting p39, 50,"

	Lampz.MassAssign(40) = l40 ' VLM.Lampz;inserts-l40
	Lampz.Callback(40) = "UpdateLightMap Parts_LM_inserts_l40, 100.0, " ' VLM.Lampz;inserts-l40
	Lampz.Callback(40) = "UpdateLightMap Gate_SpinRight_LM_inserts_l40, 100.0, " ' VLM.Lampz;inserts-l40
	Lampz.Callback(40) = "UpdateLightMap sw41_LM_inserts_l40, 100.0, " ' VLM.Lampz;inserts-l40
	Lampz.Callback(40) = "DisableLighting p40, 50,"

	Lampz.MassAssign(41) = l41 ' VLM.Lampz;inserts-l41
	Lampz.Callback(41) = "UpdateLightMap Gate_SpinRight_LM_inserts_l41, 100.0, " ' VLM.Lampz;inserts-l41
	Lampz.Callback(41) = "DisableLighting p41, 50,"

	Lampz.MassAssign(42) = l42 ' VLM.Lampz;inserts-l42
	Lampz.Callback(42) = "UpdateLightMap Parts_LM_inserts_l42, 100.0, " ' VLM.Lampz;inserts-l42
	Lampz.Callback(42) = "UpdateLightMap Gate_SpinRight_LM_inserts_l42, 100.0, " ' VLM.Lampz;inserts-l42
	Lampz.Callback(42) = "UpdateLightMap sw40_LM_inserts_l42, 100.0, " ' VLM.Lampz;inserts-l42
	Lampz.Callback(42) = "DisableLighting p42, 50,"

	Lampz.MassAssign(43) = l43
	Lampz.Callback(43) = "DisableLighting p43, 150,"

	Lampz.MassAssign(44) = l44 ' VLM.Lampz;inserts-l44
	Lampz.Callback(44) = "UpdateLightMap Parts_LM_inserts_l44, 100.0, " ' VLM.Lampz;inserts-l44
	Lampz.Callback(44) = "DisableLighting p44, 250,"

	Lampz.MassAssign(45) = l45
	Lampz.Callback(45) = "DisableLighting p45, 130,"

	Lampz.MassAssign(46) = l46
	Lampz.Callback(46) = "DisableLighting p46, 50,"

	Lampz.MassAssign(47) = l47
	Lampz.Callback(47) = "DisableLighting p47, 50,"

	Lampz.MassAssign(48) = l48 ' VLM.Lampz;inserts-l48
	Lampz.Callback(48) = "UpdateLightMap Parts_LM_inserts_l48, 100.0, " ' VLM.Lampz;inserts-l48
	Lampz.Callback(48) = "DisableLighting p48, 150,"

	Lampz.MassAssign(49) = l49 ' VLM.Lampz;inserts-l49
	Lampz.Callback(49) = "UpdateLightMap Parts_LM_inserts_l49, 100.0, " ' VLM.Lampz;inserts-l49
	Lampz.Callback(49) = "UpdateLightMap LFLogo_LM_inserts_l49, 100.0, " ' VLM.Lampz;inserts-l49
	Lampz.Callback(49) = "UpdateLightMap RFLogo_LM_inserts_l49, 100.0, " ' VLM.Lampz;inserts-l49
	Lampz.Callback(49) = "DisableLighting p49, 50,"

	Lampz.MassAssign(50) = l50 ' VLM.Lampz;inserts-l50
	Lampz.Callback(50) = "UpdateLightMap Parts_LM_inserts_l50, 100.0, " ' VLM.Lampz;inserts-l50
	Lampz.Callback(50) = "UpdateLightMap LFLogo_LM_inserts_l50, 100.0, " ' VLM.Lampz;inserts-l50
	Lampz.Callback(50) = "UpdateLightMap RFLogo_LM_inserts_l50, 100.0, " ' VLM.Lampz;inserts-l50
	Lampz.Callback(50) = "DisableLighting p50, 50,"

	Lampz.MassAssign(51) = l51 ' VLM.Lampz;inserts-l51
	Lampz.Callback(51) = "UpdateLightMap Parts_LM_inserts_l51, 100.0, " ' VLM.Lampz;inserts-l51
	Lampz.Callback(51) = "UpdateLightMap LFLogo_LM_inserts_l51, 100.0, " ' VLM.Lampz;inserts-l51
	Lampz.Callback(51) = "UpdateLightMap RFLogo_LM_inserts_l51, 100.0, " ' VLM.Lampz;inserts-l51
	Lampz.Callback(51) = "DisableLighting p51, 50,"

	Lampz.MassAssign(52) = l52 ' VLM.Lampz;inserts-l52
	Lampz.Callback(52) = "UpdateLightMap Parts_LM_inserts_l52, 100.0, " ' VLM.Lampz;inserts-l52
	Lampz.Callback(52) = "UpdateLightMap LFLogo_LM_inserts_l52, 100.0, " ' VLM.Lampz;inserts-l52
	Lampz.Callback(52) = "UpdateLightMap RFLogo_LM_inserts_l52, 100.0, " ' VLM.Lampz;inserts-l52
	Lampz.Callback(52) = "DisableLighting p52, 50,"

	Lampz.MassAssign(53) = l53 ' VLM.Lampz;inserts-l53
	Lampz.Callback(53) = "UpdateLightMap Parts_LM_inserts_l53, 100.0, " ' VLM.Lampz;inserts-l53
	Lampz.Callback(53) = "UpdateLightMap LFLogo_LM_inserts_l53, 100.0, " ' VLM.Lampz;inserts-l53
	Lampz.Callback(53) = "UpdateLightMap RFLogo_LM_inserts_l53, 100.0, " ' VLM.Lampz;inserts-l53
	Lampz.MassAssign(53) = l53
	Lampz.Callback(53) = "DisableLighting p53, 50,"

	Lampz.MassAssign(54) = l54 ' VLM.Lampz;inserts-l54
	Lampz.Callback(54) = "UpdateLightMap Layer1_LM_inserts_l54, 100.0, " ' VLM.Lampz;inserts-l54
	Lampz.Callback(54) = "UpdateLightMap Parts_LM_inserts_l54, 100.0, " ' VLM.Lampz;inserts-l54
	Lampz.Callback(54) = "DisableLighting p54, 50,"

	Lampz.MassAssign(55) = l55 ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap Parts_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap LFLogo_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap RFLogo_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap Sling_Arms_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap Sling_Arms_001_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap sw28_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap sw36_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap sw40_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap sw41_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap sw42_LM_inserts_l55, 100.0, " ' VLM.Lampz;inserts-l55
	Lampz.Callback(55) = "UpdateLightMap IMBot_LM_inserts_l55, 100.0-0.5*IMOpacity, " ' VLM.Lampz;inserts-l55
'	Lampz.Callback(55) = "DisableLighting p55, 50,"

	Lampz.MassAssign(56) = l56 ' VLM.Lampz;inserts-l56
	Lampz.Callback(56) = "UpdateLightMap Layer2_LM_inserts_l56, 100.0, " ' VLM.Lampz;inserts-l56
	Lampz.Callback(56) = "UpdateLightMap Parts_LM_inserts_l56, 100.0, " ' VLM.Lampz;inserts-l56
	Lampz.Callback(56) = "DisableLighting p56, 50,"

	Lampz.MassAssign(57) = l57 ' VLM.Lampz;inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Layer2_LM_inserts_l57, 100.0, " ' VLM.Lampz;inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Parts_LM_inserts_l57, 100.0, " ' VLM.Lampz;inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Bumpers_001_LM_inserts_l57, 100.0, " ' VLM.Lampz;inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Orbitpole_Top_LM_inserts_l57, 100.0, " ' VLM.Lampz;inserts-l57
	Lampz.Callback(57) = "UpdateLightMap sw38_LM_inserts_l57, 100.0, " ' VLM.Lampz;inserts-l57
'	Lampz.Callback(57) = "DisableLighting p57, 50,"

	Lampz.MassAssign(58) = l58 ' VLM.Lampz;inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Layer2_LM_inserts_l58, 100.0, " ' VLM.Lampz;inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Parts_LM_inserts_l58, 100.0, " ' VLM.Lampz;inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Bumpers_001_LM_inserts_l58, 100.0, " ' VLM.Lampz;inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Bumpers_003_LM_inserts_l58, 100.0, " ' VLM.Lampz;inserts-l58
	Lampz.Callback(58) = "UpdateLightMap sw38_LM_inserts_l58, 100.0, " ' VLM.Lampz;inserts-l58
	Lampz.Callback(58) = "UpdateLightMap sw39_LM_inserts_l58, 100.0, " ' VLM.Lampz;inserts-l58
'	Lampz.Callback(58) = "DisableLighting p58, 50,"

	Lampz.MassAssign(59) = l59
	Lampz.Callback(59) = "DisableLighting p59, 150,"

	Lampz.MassAssign(60) = l60 ' VLM.Lampz;inserts-l60
	Lampz.Callback(60) = "UpdateLightMap Parts_LM_inserts_l60, 100.0, " ' VLM.Lampz;inserts-l60
	Lampz.Callback(60) = "UpdateLightMap Orbitpole_Top_LM_inserts_l60, 100.0, " ' VLM.Lampz;inserts-l60

	Lampz.MassAssign(61) = l61 ' VLM.Lampz;inserts-l61
	Lampz.Callback(61) = "UpdateLightMap Layer2_LM_inserts_l61, 100.0, " ' VLM.Lampz;inserts-l61
	Lampz.Callback(61) = "UpdateLightMap Parts_LM_inserts_l61, 100.0, " ' VLM.Lampz;inserts-l61
	Lampz.Callback(61) = "UpdateLightMap sw39_LM_inserts_l61, 100.0, " ' VLM.Lampz;inserts-l61

	Lampz.MassAssign(62) = l62 ' VLM.Lampz;inserts-l62
	Lampz.Callback(62) = "UpdateLightMap Layer2_LM_inserts_l62, 100.0, " ' VLM.Lampz;inserts-l62
	Lampz.Callback(62) = "UpdateLightMap Parts_LM_inserts_l62, 100.0, " ' VLM.Lampz;inserts-l62

	Lampz.MassAssign(63) = l63 ' VLM.Lampz;inserts-l63
	Lampz.Callback(63) = "UpdateLightMap Layer1_LM_inserts_l63, 100.0, " ' VLM.Lampz;inserts-l63
	Lampz.Callback(63) = "UpdateLightMap Parts_LM_inserts_l63, 100.0, " ' VLM.Lampz;inserts-l63
	Lampz.Callback(63) = "UpdateLightMap Gate_SpinRight_LM_inserts_l63, 100.0, " ' VLM.Lampz;inserts-l63
	Lampz.Callback(63) = "UpdateLightMap sw40_LM_inserts_l63, 100.0, " ' VLM.Lampz;inserts-l63
	Lampz.Callback(63) = "DisableLighting p63, 150,"

'insert top texts
    Lampz.MassAssign(3) = l3a

	Lampz.MassAssign(4) = l4r
	Lampz.MassAssign(5) = l5r
	Lampz.MassAssign(6) = l6r
	Lampz.MassAssign(7) = l7r

    Lampz.MassAssign(8) = l8a
	Lampz.MassAssign(12) = l12a
	Lampz.MassAssign(13) = l13a
    Lampz.MassAssign(15) = l15a
	Lampz.MassAssign(17) = l17a
	Lampz.MassAssign(18) = l18a
	Lampz.MassAssign(19) = l19a
	Lampz.MassAssign(20) = l20a
   	Lampz.MassAssign(21) = l21a
	Lampz.MassAssign(22) = l22a
	Lampz.MassAssign(23) = l23a
	Lampz.MassAssign(24) = l24a
	Lampz.MassAssign(25) = l25a
	Lampz.MassAssign(28) = l28a
	Lampz.MassAssign(29) = l29a
	Lampz.MassAssign(30) = l30a
	Lampz.MassAssign(31) = l31a
	Lampz.MassAssign(32) = l32a
	Lampz.MassAssign(33) = l33a
    Lampz.MassAssign(35) = l35a
	Lampz.MassAssign(36) = l36a
	Lampz.MassAssign(37) = l37a
	Lampz.MassAssign(38) = l38a
	Lampz.MassAssign(39) = l39a
    Lampz.MassAssign(40) = l40a
	Lampz.MassAssign(41) = l41a
    Lampz.MassAssign(42) = l42a
	Lampz.MassAssign(43) = l43a
	Lampz.MassAssign(44) = l44a
	Lampz.MassAssign(45) = l45a
	Lampz.MassAssign(46) = l46a
	Lampz.MassAssign(47) = l47a
	Lampz.MassAssign(48) = l48a
	Lampz.MassAssign(49) = l49a
    Lampz.MassAssign(50) = l50a
    Lampz.MassAssign(51) = l51a
    Lampz.MassAssign(52) = l52a
    Lampz.MassAssign(53) = l53a
    Lampz.MassAssign(56) = l56a
	Lampz.MassAssign(59) = l59a

'ball reflection lamps
	Lampz.MassAssign(4) = l4r
	Lampz.MassAssign(5) = l5r
	Lampz.MassAssign(6) = l6r
	Lampz.MassAssign(7) = l7r
	Lampz.MassAssign(9) = l9r
	Lampz.MassAssign(10) = l10r
	Lampz.MassAssign(11) = l11r
	Lampz.MassAssign(14) = l14r
	Lampz.MassAssign(16) = l16r
	Lampz.MassAssign(26) = l26r
	Lampz.MassAssign(27) = l27r
	Lampz.MassAssign(34) = l34r
	Lampz.MassAssign(54) = l54r
	Lampz.MassAssign(55) = l55r
	Lampz.MassAssign(57) = l57r
	Lampz.MassAssign(58) = l58r
	Lampz.MassAssign(63) = l63r
	Lampz.MassAssign(122) = l122r
	Lampz.MassAssign(123) = l123r
	Lampz.MassAssign(127) = l127r
	Lampz.MassAssign(120) = l120r

	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	lampz.update
End Sub

Sub SetLampMod(id, val)
	'Debug.print "> " & id & " => " & val
	Lampz.state(id) = val
End Sub

'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

Sub FadeMaterialColor(PrimName, LevelMax, LevelMin, aLvl)
	dim rgbLevel
	rgbLevel = ((LevelMax - LevelMin) * aLvl) + LevelMin

    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial PrimName.material, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    UpdateMaterial PrimName.material, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, rgb(rgbLevel,rgbLevel,rgbLevel), glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

Sub UpdateLightMap(lightmap, intensity, ByVal aLvl)
   if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
   lightmap.Opacity = aLvl * intensity
End Sub

'**********************************************************************
'////////////////////////////---   GI  ---////////////////////////////'
'**********************************************************************

dim gilvl : gilvl=0

set GICallback = GetRef("UpdateGI")

Sub UpdateGI(nr,enabled)
	Select Case nr
		Case 0
		If Enabled Then
			GIOn
			If VRBackglassGI = 1 and VRRoom <> 0 Then
				For each VRthings in VRBGGI: VRthings.visible = 1 : Next
			End If
		Else	
			GIOff
			If VRBackglassGI = 1 and VRRoom <> 0 Then
				For each VRthings in VRBGGI: VRthings.visible = 0 : Next
			End If
		End If	
	End Select
End Sub

Sub GIOn
	dim bulb
	PlaySound "Relay_On"
	gilvl = 1
	Lampz.state(104) = 1
End Sub

Sub GIOff
	dim bulb
	PlaySound "Relay_Off"
	gilvl = 0
	Lampz.state(104) = 0
End Sub


'**********************************************************************
'//////////////////////////---   GI End  ---//////////////////////////'
'**********************************************************************


'**********************************************************************
'Class jungle nf
'**********************************************************************

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks
'version 0.013b - Updated with larger size and support for modulated solenoids

Class LampFader
	Public FadeSpeedDown(150), FadeSpeedUp(150)
	Private Lock(150), Loaded(150), OnOff(150)
	Public UseFunction
	Private cFilter
	Public UseCallback(150), cCallback(150)
	Public Lvl(150), Obj(150)
	Private Mult(150)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = 0
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if TypeName(input) <> "Double" and typename(input) <> "Integer"  and typename(input) <> "Long" then
			If input Then
				input = 1
			Else
				input = 0
			End If
		End If
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		''debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) > 0 then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= OnOff(x) then Lvl(x) = OnOff(x) : Lock(x) = True
				else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) > 0 then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= OnOff(x) then Lvl(x) = OnOff(x) : Lock(x) = True
				else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx, aLvl : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				aLvl = Lvl(x)*Mult(x)
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(aLvl) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = aLvl : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(aLvl)
					Else
						obj(x).Intensityscale = aLvl
					End If
				end if
				'if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" and typename(lvl(x)) <> "Long" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,aLvl	'Proc
				If Lock(x) Then
					if Lvl(x) = OnOff(x) or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class

'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function

'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

'******************************************************
'****  END LAMPZ
'******************************************************



'***********************************************************************
'///////////////////---   Iron Monger Animation   ---///////////////////
'***********************************************************************

Dim MongerMax:MongerMax = 240
Dim MongerSpeed:MongerSpeed = MongerMax / 62
'Dim MongerSpeed:MongerSpeed = MongerMax / 1000
Dim monger, mongerPos, mongerDir, mongerFlash, imf
mongerDir = 0:mongerPos = MongerMax:mongerFlash = 0

sw4.IsDropped = 1
sw5.IsDropped = 1
sw6.IsDropped = 1
switchframe.IsDropped = 1
 
Sub Solmonger(Enabled)
	If Enabled Then
		If mongerDir = 0 Then
			Controller.Switch(1) = 1
			Controller.Switch(3) = 0
			mongerOpen.Enabled = 0
			mongerClose.Enabled = 1
			PlaySoundAtLevelStatic "Motor_DW", 0.15, IMTop_LM_GI
			imongerblocker.TimerEnabled = False
			imongerblocker.IsDropped=1
			mongerClose_Timer
			mongerDir = 1
		Else
            dim b: for each b in gBOT  'kick ball out of way if on top of ironmonger
                if InRect(b.x,b.y,340,825,510,825,510,970,340,970) then
                    b.velz = 20
                    b.vely = b.vely + 5
                end if
            next
			Controller.Switch(1) = 0
			Controller.Switch(3) = 1
			mongerOpen.Enabled = 1
			mongerClose.Enabled = 0
			PlaySoundAtLevelStatic "Motor_DW", 0.15, IMTop_LM_GI
			imongerblocker.TimerEnabled = True
			mongerOpen_Timer
			mongerDir = 0
		End If
	End If
End Sub

imongerblocker.TimerInterval = 20
Sub imongerblocker_timer
    imongerblocker.IsDropped=0
    imongerblocker.TimerEnabled = False
End Sub

Sub mongerClose_Timer()
	mongerPos = mongerPos + MongerSpeed
	If mongerPos> MongerMax Then
		mongerPos = MongerMax
		mongerClose.Enabled = 0
		switchframe.IsDropped = 1:sw4.IsDropped = 1:sw5.IsDropped = 1:sw6.IsDropped = 1
		for each imf in iml125flash:imf.visible = 0:Next
		for each imf in iml128flash:imf.visible = 0:Next
		Stopsound "Motor_DW"
	End If
	Updatemonger
End Sub

Sub mongerOpen_Timer()
	mongerPos = mongerPos - MongerSpeed
	If mongerPos <0 Then
		mongerPos = 0
		mongerOpen.Enabled = 0
		switchframe.IsDropped = 0:sw4.IsDropped = 0:sw5.IsDropped = 0:sw6.IsDropped = 0
		for each imf in iml125flash:imf.visible = 1:Next
		for each imf in iml128flash:imf.visible = 1:Next
		IMTopUpped_BM_Dark_Room.visible = true				'visiblity swap to prevent z-fighting
		IMTop_BM_Dark_Room.visible = false
		Stopsound "Motor_DW"
	End If
	Updatemonger
End Sub

dim IMOpacity
Updatemonger

Sub Updatemonger
	DIM im, z

	'calculating opacity value that is referenced in this sub and in lampz callbacks
	IMOpacity = (100.0 * mongerpos) / MongerMax	'up = 0 = imtopupped visible, down = 1 = imtop visible

	z = MongerMax-MongerPos
	' debug.print z

	' Only have one of the both solid bake visible ta a time (swap at the mid path)
	IMTop_BM_Dark_Room.visible	= IMOpacity >= 50.0
	IMTopUpped_BM_Dark_Room.visible	= IMOpacity < 50.0

	'imbot objects mapped to their lamp ID's
	IMBot_BM_Dark_Room.z = z ' VLM.Props;BM;1;IM.001
	IMBot_LM_Lit_Room.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_GI.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l120.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l121.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l122.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l123.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l125.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l126.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l127.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l128.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l129.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l130.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l131.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_flashers_l132.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_inserts_l23.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_inserts_l24.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_inserts_l27.z = z ' VLM.Props;LM;1;IM.001
	IMBot_LM_inserts_l55.z = z ' VLM.Props;LM;1;IM.001

	'height update for imtop objects
	IMTop_BM_Dark_Room.z = z ' VLM.Props;BM;1;IMPlastics
	IMTop_LM_Lit_Room.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_GI.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l120.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l121.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l122.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l123.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l126.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l127.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l129.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l131.z = z ' VLM.Props;LM;1;IMPlastics
	IMTop_LM_flashers_l132.z = z ' VLM.Props;LM;1;IMPlastics

	'height update for imtopUpped objects
	IMTopUpped_BM_Dark_Room.z = z ' VLM.Props;BM;1;IMPlastics.003
	IMTopUpped_LM_Lit_Room.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_GI.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l120.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l122.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l123.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l125.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l126.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l127.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l128.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l129.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l130.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l131.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_flashers_l132.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_inserts_l13.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_inserts_l22.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_inserts_l23.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_inserts_l24.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_inserts_l25.z = z ' VLM.Props;LM;1;IMPlastics.003
	IMTopUpped_LM_inserts_l27.z = z ' VLM.Props;LM;1;IMPlastics.003
	
	'imbot objects mapped to their lamp ID's
	IMBot_LM_GI.opacity               = (100.0-0.5*IMOpacity) * lampz.lvl(104)
	IMBot_LM_Lit_Room.opacity         = (100.0-0.5*IMOpacity) * lampz.lvl(150) / 100
	IMBot_LM_flashers_l120.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(120) / 255
	IMBot_LM_flashers_l121.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(121) / 255
	IMBot_LM_flashers_l122.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(122) / 255
	IMBot_LM_flashers_l123.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(123) / 255
	IMBot_LM_flashers_l125.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(125) / 255
	IMBot_LM_flashers_l126.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(126) / 255
	IMBot_LM_flashers_l127.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(127) / 255
	IMBot_LM_flashers_l128.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(128) / 255
	IMBot_LM_flashers_l129.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(129) / 255
	IMBot_LM_flashers_l130.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(130) / 255
	IMBot_LM_flashers_l131.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(131) / 255
	IMBot_LM_flashers_l132.opacity    = (100.0-0.5*IMOpacity) * lampz.lvl(132) / 255
	IMBot_LM_inserts_l23.opacity      = (100.0-0.5*IMOpacity) * lampz.lvl(23)
	IMBot_LM_inserts_l24.opacity      = (100.0-0.5*IMOpacity) * lampz.lvl(24)
	IMBot_LM_inserts_l27.opacity      = (100.0-0.5*IMOpacity) * lampz.lvl(27)
	IMBot_LM_inserts_l55.opacity      = (100.0-0.5*IMOpacity) * lampz.lvl(55)

 	'imtop objects mapped to their lamp ID's
	IMTop_LM_GI.opacity                 = IMOpacity * lampz.lvl(104)
	IMTop_LM_Lit_Room.opacity           = IMOpacity * lampz.lvl(150) / 100
	IMTop_LM_flashers_l120.opacity      = IMOpacity * lampz.lvl(120) / 255
	IMTop_LM_flashers_l121.opacity      = IMOpacity * lampz.lvl(121) / 255
	IMTop_LM_flashers_l122.opacity      = IMOpacity * lampz.lvl(122) / 255
	IMTop_LM_flashers_l123.opacity      = IMOpacity * lampz.lvl(123) / 255
	IMTop_LM_flashers_l126.opacity      = IMOpacity * lampz.lvl(126) / 255
	IMTop_LM_flashers_l127.opacity      = IMOpacity * lampz.lvl(127) / 255
	IMTop_LM_flashers_l129.opacity      = IMOpacity * lampz.lvl(129) / 255
	IMTop_LM_flashers_l131.opacity      = IMOpacity * lampz.lvl(131) / 255
	IMTop_LM_flashers_l132.opacity      = IMOpacity * lampz.lvl(132) / 255

 	'imtopUpped objects mapped to their lamp ID's
	IMTopUpped_LM_GI.opacity               = (100 - IMOpacity) * lampz.lvl(104)
	IMTopUpped_LM_Lit_Room.opacity         = (100 - IMOpacity) * lampz.lvl(150) / 100
	IMTopUpped_LM_flashers_l120.opacity    = (100 - IMOpacity) * lampz.lvl(120) / 255
	IMTopUpped_LM_flashers_l122.opacity    = (100 - IMOpacity) * lampz.lvl(122) / 255
	IMTopUpped_LM_flashers_l123.opacity    = (100 - IMOpacity) * lampz.lvl(123) / 255
	IMTopUpped_LM_flashers_l125.opacity    = (100 - IMOpacity) * lampz.lvl(125) / 255
	IMTopUpped_LM_flashers_l126.opacity    = (100 - IMOpacity) * lampz.lvl(126) / 255
	IMTopUpped_LM_flashers_l127.opacity    = (100 - IMOpacity) * lampz.lvl(127) / 255
	IMTopUpped_LM_flashers_l128.opacity    = (100 - IMOpacity) * lampz.lvl(128) / 255
	IMTopUpped_LM_flashers_l129.opacity    = (100 - IMOpacity) * lampz.lvl(129) / 255
	IMTopUpped_LM_flashers_l130.opacity    = (100 - IMOpacity) * lampz.lvl(130) / 255
	IMTopUpped_LM_flashers_l131.opacity    = (100 - IMOpacity) * lampz.lvl(131) / 255
	IMTopUpped_LM_flashers_l132.opacity    = (100 - IMOpacity) * lampz.lvl(132) / 255
	IMTopUpped_LM_inserts_l13.opacity      = (100 - IMOpacity) * lampz.lvl(13)
	IMTopUpped_LM_inserts_l22.opacity      = (100 - IMOpacity) * lampz.lvl(22)
	IMTopUpped_LM_inserts_l23.opacity      = (100 - IMOpacity) * lampz.lvl(23)
	IMTopUpped_LM_inserts_l24.opacity      = (100 - IMOpacity) * lampz.lvl(24)
	IMTopUpped_LM_inserts_l25.opacity      = (100 - IMOpacity) * lampz.lvl(25)
	IMTopUpped_LM_inserts_l27.opacity      = (100 - IMOpacity) * lampz.lvl(27)
End Sub


'**********************************************************************
'/////////////////---   Iron Monger Animation End  ---/////////////////
'**********************************************************************



'**********************************************************************
'/////////////////////////---   Physics  ---//////////////////////////'
'**********************************************************************

'***********************************************************************
' Begin NFozzy Physics Scripting:  Flipper Tricks and Rubber Dampening '
'***********************************************************************

'****************************************************************
'	Flipper Collision Subs
'****************************************************************

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************

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

' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function

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

'****************************************************************
'	FLIPPER CORRECTION INITIALIZATION
'****************************************************************

Const LiveCatch = 20

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5.5
	AddPt "Polarity", 2, 0.4, -5.5
	AddPt "Polarity", 3, 0.6, -5.0
	AddPt "Polarity", 4, 0.65, -4.5
	AddPt "Polarity", 5, 0.7, -4.0
	AddPt "Polarity", 6, 0.75, -3.5
	AddPt "Polarity", 7, 0.8, -3.0
	AddPt "Polarity", 8, 0.85, -2.5
	AddPt "Polarity", 9, 0.9,-2.0
	AddPt "Polarity", 10, 0.95, -1.5
	AddPt "Polarity", 11, 1, -1.0
	AddPt "Polarity", 12, 1.05, -0.5
	AddPt "Polarity", 13, 1.1, 0
	AddPt "Polarity", 14, 1.3, 0

	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'****************************************************************
'	FLIPPER CORRECTION FUNCTIONS
'****************************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
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
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'****************************************************************
'	FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'****************************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
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
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b', BOT
'	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'****************************************************************
'	Check ball distance from Flipper for Rem
'****************************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If	
End Function

'****************************************************************
'	End - Check ball distance from Flipper for Rem
'****************************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

'Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025  'mid 90's and later

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
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b', BOT
'		BOT = GetBalls

		For b = 0 to UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= -0.4 Then gBOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub

'****************************************************************
'	PHYSICS DAMPENERS
'****************************************************************
'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

'sling bottom corners
Sub zCol_Rubber_Post004_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub zCol_Rubber_Post005_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class

'****************************************************************
'	TRACK ALL BALL VELOCITIES FOR RUBBER DAMPENER AND DROP TARGETS
'****************************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = gBOT

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
        Dim AB, BC, CD, DA
        AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
        BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
        CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
        DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
        If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
                InRect = True
        Else
                InRect = False       
        End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
    Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
    Dim rotxy
    rotxy = RotPoint(ax,ay,angle)
    rax = rotxy(0)+px : ray = rotxy(1)+py
    rotxy = RotPoint(bx,by,angle)
    rbx = rotxy(0)+px : rby = rotxy(1)+py
    rotxy = RotPoint(cx,cy,angle)
    rcx = rotxy(0)+px : rcy = rotxy(1)+py
    rotxy = RotPoint(dx,dy,angle)
    rdx = rotxy(0)+px : rdy = rotxy(1)+py

    InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

'*********************************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'*********************************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.8 	'Level of bounces. Recommmended value of 0.7

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

'*********************************************************************
' Narnia Catcher
'*********************************************************************

Sub Narnia_Timer
	Dim b : For b = 0 to UBound(gBOT)
		'Check for narnia balls
		If gBOT(b).z < -200 Then
			'msgbox "Ball " &b& " in Narnia X: " & gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z
			'debug.print "Move narnia ball ("& gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z&") to plunger lane"
			gBOT(b).x = 900 : gBOT(b).y = 1900 : gBOT(b).z = 26
			gBOT(b).velx = 0 : gBOT(b).vely = 0 : gBOT(b).velz = 0
		end if
	Next
End Sub


'******************************************************
'		STAND-UP TARGET INITIALIZATION
'******************************************************

'Define a variable for each stand-up target
Dim ST33, ST34, ST35, ST36, ST40, ST41, ST42, ST44, ST45, ST46, ST47, ST48, ST50

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

ST33 = Array(sw33, sw33_BM_Dark_Room,33,0)
ST34 = Array(sw34, sw34_BM_Dark_Room,34,0)
ST35 = Array(sw35, sw35_BM_Dark_Room,35,0)
ST36 = Array(sw36, sw36_BM_Dark_Room,36,0)
ST40 = Array(sw40, sw40_BM_Dark_Room,40,0)
ST41 = Array(sw41, sw41_BM_Dark_Room,41,0)
ST42 = Array(sw42, sw42_BM_Dark_Room,42,0)
ST44 = Array(sw44, sw44_BM_Dark_Room,44,0)
ST45 = Array(sw45, sw45_BM_Dark_Room,45,0)
ST46 = Array(sw46, sw46_BM_Dark_Room,46,0)
ST47 = Array(sw47, sw47_BM_Dark_Room,47,0)
ST48 = Array(sw48, sw48_BM_Dark_Room,48,0)
ST50 = Array(sw50, sw50_BM_Dark_Room,50,0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array (ST33, ST34, ST35, ST36, ST40, ST41, ST42, ST44, ST45, ST46, ST47, ST48, ST50)

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
	STArray(i)(3) =  STCheckHit(Activeball,STArray(i)(0))

	If STArray(i)(3) <> 0 Then
		DTBallPhysics Activeball, STArray(i)(0).orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i)(2) = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180	
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		STCheckHit = 1
	Else 
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i)(3) = STAnimate(STArray(i)(0),STArray(i)(1),STArray(i)(2),STArray(i)(3))
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime

	STAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If animate = 1 Then
		primary.collidable = 0
		prim.transy = -STMaxOffset
		vpmTimer.PulseSw switch
		STAnimate = 2
		Exit Function
	elseif animate = 2 Then
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

sub DTBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3
End Sub

'****************************************************************
'	END nFozzy Physics
'****************************************************************


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iaakki, Apophis, and Wylte
'***************************************************************

Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(4), objrtx2(4)
dim objBallShadow(4)
dim objSpotShadow1(4), objSpotShadow2(4)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.31			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.32
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.34
		objBallShadow(iii).visible = 0

		Set objSpotShadow1(iii) = Eval("SpotlightShadow" & iii)
		objSpotShadow1(iii).material = "BallSpotShadow" & iii
		objSpotShadow1(iii).Z = iii/1000 + 0.36
		objSpotShadow1(iii).visible = 0
		Set objSpotShadow2(iii) = Eval("Spotlight2Shadow" & iii)
		objSpotShadow2(iii).material = "BallSpot2Shadow" & iii
		objSpotShadow2(iii).Z = iii/1000 + 0.37
		objSpotShadow2(iii).visible = 0


		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub

' *** These define the appearance of shadows in your table
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at 25+ hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source
Const fovY					= 0		'Offset y position under ball to account for layback or inclination (personal preference)
Const falloff   			= 150 	'Max distance to light sources, can be changed if you have a reason
Const Spotfalloff 			= 200
Dim GIfalloff : GIfalloff   = 250

Const PLOffset = 0.1
Dim PLGain: PLGain = (1-PLOffset)/(1260-2100)

Sub DynamicBSUpdate
	Dim ShadowOpacity, ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, LSd1, LSd2, iii
	Dim dist1, dist2, src1, src2
'	Dim BOT: BOT=getballs

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	Dim l
	Dim d_w
	Dim b_base, b_r, b_g, b_b
	b_base = 150 * 0.01 * Parts_LM_GI.Opacity + 55 * (LightLevel / 100) + 10
	For s = lob to UBound(gBOT)

' *** Compute ball lighting from GI and ambient lighting
		d_w = GIfalloff
		For Each l in GILights
			LSd = Distance(gBOT(s).x, gBOT(s).y, l.x, l.y) 'Calculating the Linear distance to the Source
			If LSd < d_w then d_w = LSd
		Next
		d_w = b_base + 70 * (1 - d_w / GIfalloff) * 0.01 * Parts_LM_GI.Opacity
		' Handle plunger lane
		If InRect(gBOT(s).x,gBOT(s).y,870,2100,870,1260,930,1260,930,2100) Then 
			d_w = d_w*(PLOffset+PLGain*(gBOT(s).y-2100))
		End If
		' Assign color
		b_r = Int(d_w)
		b_g = Int(d_w)
		b_b = Int(d_w)
		If b_r > 255 Then b_r = 255
		If b_g > 255 Then b_g = 255
		If b_b > 255 Then b_b = 255
		gBot(s).color = b_r + (b_g * 256) + (b_b * 256 * 256)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = gBOT(s).X
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Spotlight Shadows
		If SpotlightShadowsOn = 1 Then
			If InRect(gBOT(s).x,gBOT(s).y,232,1468,139,1115,400,1156,583,1376) Then 	'Defining where the spotlight is in effect
				objSpotShadow2(s).visible = 0
				LSd1=Distance(gBOT(s).x, gBOT(s).y, Lspot1.x, Lspot1.y)
				If LSd1 < Spotfalloff And Lspot1.state=1 Then
					objSpotShadow1(s).visible = 1 : objSpotShadow1(s).X = gBOT(s).X : objSpotShadow1(s).Y = gBOT(s).Y + fovY
					objSpotShadow1(s).rotz = AnglePP(Lspot1.x, Lspot1.y, gBOT(s).X, gBOT(s).Y) + 90	'Had to use custom coordinates, light and primitive are shifted
					objSpotShadow1(s).size_y = Wideness/2
					ShadowOpacity = (Spotfalloff-LSd1)/Spotfalloff			'Sets opacity/darkness of shadow by distance to light
					UpdateMaterial objSpotShadow1(s).material,1,0,0,0,0,0,0.5*ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0

				Else
					objSpotShadow1(s).visible = 0
				End If

			ElseIf InRect(gBOT(s).x,gBOT(s).y,691,1502,354,1382,536,1179,812,1160) Then 	'Defining where the spotlight is in effect
				objSpotShadow1(s).visible = 0
				LSd2=Distance(gBOT(s).x, gBOT(s).y, Lspot2.x, Lspot2.y)
				If LSd2 < Spotfalloff And Lspot2.state=1 Then
					objSpotShadow2(s).visible = 1 : objSpotShadow2(s).X = gBOT(s).X : objSpotShadow2(s).Y = gBOT(s).Y + fovY
					objSpotShadow2(s).rotz = AnglePP(Lspot2.x, Lspot2.y, gBOT(s).X, gBOT(s).Y) + 90
					objSpotShadow2(s).size_y = Wideness/2
					ShadowOpacity = (Spotfalloff-LSd2)/Spotfalloff			'Sets opacity/darkness of shadow by distance to light
					UpdateMaterial objSpotShadow2(s).material,1,0,0,0,0,0,0.5*ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0

				Else
					objSpotShadow2(s).visible = 0
				End If
			Else
				objSpotShadow1(s).visible = 0 : objSpotShadow2(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn = 1 Then
			If gBOT(s).Z < 30 Then
				dist1 = falloff
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < dist1 And gilvl > 0 Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y + fovY
					'objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + fovY
					'objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iaakki, Apophis, and Wylte
'****************************************************************



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
	Dim b', BOT
'	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
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

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = gBOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************



'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(5,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(5)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function


'Ramp triggers
Sub RampTrigger1_Hit
	If activeball.vely < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
End Sub

Sub RampTrigger2_Hit
	If activeball.vely < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
End Sub

Sub RampTrigger3_Hit
    if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 50
    activeball.AngMomZ = -abs(activeball.AngMomZ) * 3
    WireRampOff
End Sub

Sub RampTrigger4_Hit
    if abs(activeball.AngMomZ) > 70 then activeball.AngMomZ = 50
    activeball.AngMomZ = abs(activeball.AngMomZ) * 3
    WireRampOff
End Sub

Sub RampTrigger5_Hit
	If activeball.vely < 0 Then
		WireRampOn True
	Else
		WireRampOff
	End If
End Sub

Sub RampTrigger6_Hit
	WireRampOff
End Sub



'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************



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
Dim SaucerLockSoundLevel, SaucerKickSoundLevel, PostSoundLevel

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
PostSoundLevel = 0.8

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

Sub Apron_Hit (idx)
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

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************


'***********************************************************************
'* TABLE OPTIONS *******************************************************
'***********************************************************************

Dim LightLevel : LightLevel = 50
Dim ColorLUT : ColorLUT = 1
Dim OutPostMod : OutPostMod = 0
Dim VolumeDial : VolumeDial = 0.8			'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5 	'Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 	'Level of ramp rolling volume. Value between 0 and 1

Dim Cabinetmode	: Cabinetmode = 0			'0 - Siderails On, 1 - Siderails Off
Dim IncandescentGI : IncandescentGI = 0     '0 - LED GI, 1 - Incandescent GI

Dim DynamicBallShadowsOn : DynamicBallShadowsOn = 1	  '0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Dim SpotlightShadowsOn : SpotlightShadowsOn = 1		  'Toggle for shadows from spotlights
Dim AmbientBallShadowOn : AmbientBallShadowOn = 1	  '0 = Static shadow under ball ("flasher" image, like JP's)
									                  '1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									                  '2 = flasher image shadow, but it moves like ninuzzu's

Dim VRRoomChoice : VRRoomChoice = 0				'0 - Minimal Room, 1 - Ultra Minimal Room (only applies when using VR headset)
Dim VRBackglassGI : VRBackglassGI = 0			'0 - Static lighting (emulates real Table), 1 - Backglass GI turns on/off with table
Dim VRSideBlades : VRSideBlades = 0				'0 - Off, 1 - On (sideblade art, but lose sideblade reflections)
Dim VRTopperOn : VRTopperOn = 1				    '0 - Off, 1 - On

' Base options
Const Opt_Light = 0
Const Opt_LUT = 1
Const Opt_Outpost = 2
Const Opt_Volume = 3
Const Opt_Volume_Ramp = 4
Const Opt_Volume_Ball = 5
' Table mods & toys
Const Opt_Cabinet = 6
Const Opt_IncandescentGI = 7
' Shadow options
Const Opt_DynBallShadow = 8
Const Opt_SpotlightShadow = 9
Const Opt_AmbientBallShadow = 10
' VR options
Const Opt_VRRoomChoice = 11
Const Opt_VRBackglassGI = 12
Const Opt_VRSideBlades = 13
Const Opt_VRTopperOn = 14
' Informations
Const Opt_Info_1 = 15
Const Opt_Info_2 = 16

Const NOptions = 17

Const FlexDMD_RenderMode_DMD_GRAY_2 = 0
Const FlexDMD_RenderMode_DMD_GRAY_4 = 1
Const FlexDMD_RenderMode_DMD_RGB = 2
Const FlexDMD_RenderMode_SEG_2x16Alpha = 3
Const FlexDMD_RenderMode_SEG_2x20Alpha = 4
Const FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num = 5
Const FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num_4x1Num = 6
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num = 7
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_10x1Num = 8
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num_gen7 = 9
Const FlexDMD_RenderMode_SEG_2x7Num10_2x7Num10_4x1Num = 10
Const FlexDMD_RenderMode_SEG_2x6Num_2x6Num_4x1Num = 11
Const FlexDMD_RenderMode_SEG_2x6Num10_2x6Num10_4x1Num = 12
Const FlexDMD_RenderMode_SEG_4x7Num10 = 13
Const FlexDMD_RenderMode_SEG_6x4Num_4x1Num = 14
Const FlexDMD_RenderMode_SEG_2x7Num_4x1Num_1x16Alpha = 15
Const FlexDMD_RenderMode_SEG_1x16Alpha_1x16Num_1x7Num = 16

Const FlexDMD_Align_TopLeft = 0
Const FlexDMD_Align_Top = 1
Const FlexDMD_Align_TopRight = 2
Const FlexDMD_Align_Left = 3
Const FlexDMD_Align_Center = 4
Const FlexDMD_Align_Right = 5
Const FlexDMD_Align_BottomLeft = 6
Const FlexDMD_Align_Bottom = 7
Const FlexDMD_Align_BottomRight = 8

Const FlexDMD_Scaling_Fit = 0
Const FlexDMD_Scaling_Fill = 1
Const FlexDMD_Scaling_FillX = 2
Const FlexDMD_Scaling_FillY = 3
Const FlexDMD_Scaling_Stretch = 4
Const FlexDMD_Scaling_StretchX = 5
Const FlexDMD_Scaling_StretchY = 6
Const FlexDMD_Scaling_None = 7

Const FlexDMD_Interpolation_Linear = 0
Const FlexDMD_Interpolation_ElasticIn = 1
Const FlexDMD_Interpolation_ElasticOut = 2
Const FlexDMD_Interpolation_ElasticInOut = 3
Const FlexDMD_Interpolation_QuadIn = 4
Const FlexDMD_Interpolation_QuadOut = 5
Const FlexDMD_Interpolation_QuadInOut = 6
Const FlexDMD_Interpolation_CubeIn = 7
Const FlexDMD_Interpolation_CubeOut = 8
Const FlexDMD_Interpolation_CubeInOut = 9
Const FlexDMD_Interpolation_QuartIn = 10
Const FlexDMD_Interpolation_QuartOut = 11
Const FlexDMD_Interpolation_QuartInOut = 12
Const FlexDMD_Interpolation_QuintIn = 13
Const FlexDMD_Interpolation_QuintOut = 14
Const FlexDMD_Interpolation_QuintInOut = 15
Const FlexDMD_Interpolation_SineIn = 16
Const FlexDMD_Interpolation_SineOut = 17
Const FlexDMD_Interpolation_SineInOut = 18
Const FlexDMD_Interpolation_BounceIn = 19
Const FlexDMD_Interpolation_BounceOut = 20
Const FlexDMD_Interpolation_BounceInOut = 21
Const FlexDMD_Interpolation_CircIn = 22
Const FlexDMD_Interpolation_CircOut = 23
Const FlexDMD_Interpolation_CircInOut = 24
Const FlexDMD_Interpolation_ExpoIn = 25
Const FlexDMD_Interpolation_ExpoOut = 26
Const FlexDMD_Interpolation_ExpoInOut = 27
Const FlexDMD_Interpolation_BackIn = 28
Const FlexDMD_Interpolation_BackOut = 29
Const FlexDMD_Interpolation_BackInOut = 30

Dim OptionDMD: Set OptionDMD = Nothing
Dim bOptionsMagna, bInOptions : bOptionsMagna = False
Dim OptPos, OptSelected, OptN, OptTop, OptBot, OptSel
Dim OptFontHi, OptFontLo

Sub Options_Open
	bOptionsMagna = False
	Set OptionDMD = CreateObject("FlexDMD.FlexDMD")
	If OptionDMD is Nothing Then
		Debug.Print "FlexDMD is not installed"
		Debug.Print "Option UI can not be opened"
		Exit Sub
	End If
	Debug.Print "Option UI opened"
	If ShowDT Then OptionDMDFlasher.RotX = -(Table.Inclination + Table.Layback)
	bInOptions = True
	OptPos = 0
	OptSelected = False
	OptionDMD.Show = False
	OptionDMD.RenderMode = FlexDMD_RenderMode_DMD_GRAY_4
	OptionDMD.Width = 128
	OptionDMD.Height = 32
	OptionDMD.Clear = True
	OptionDMD.Run = True
	Dim a, scene, font
	Set scene = OptionDMD.NewGroup("Scene")
	Set OptFontHi = OptionDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbWhite, vbWhite, 0)
	Set OptFontLo = OptionDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(100, 100, 100), RGB(100, 100, 100), 0)
	Set OptSel = OptionDMD.NewGroup("Sel")
	Set a = OptionDMD.NewLabel(">", OptFontLo, ">>>")
	a.SetAlignedPosition 1, 16, FlexDMD_Align_Left
	OptSel.AddActor a
	Set a = OptionDMD.NewLabel(">", OptFontLo, "<<<")
	a.SetAlignedPosition 127, 16, FlexDMD_Align_Right
	OptSel.AddActor a
	scene.AddActor OptSel
	OptSel.SetBounds 0, 0, 128, 32
	OptSel.Visible = False
	
	Set a = OptionDMD.NewLabel("Info1", OptFontLo, "MAGNA EXIT/ENTER")
	a.SetAlignedPosition 1, 32, FlexDMD_Align_BottomLeft
	scene.AddActor a
	Set a = OptionDMD.NewLabel("Info2", OptFontLo, "FLIPPER SELECT")
	a.SetAlignedPosition 127, 32, FlexDMD_Align_BottomRight
	scene.AddActor a
	Set OptN = OptionDMD.NewLabel("Pos", OptFontLo, "LINE 1")
	Set OptTop = OptionDMD.NewLabel("Top", OptFontLo, "LINE 1")
	Set OptBot = OptionDMD.NewLabel("Bottom", OptFontLo, "LINE 2")
	scene.AddActor OptN
	scene.AddActor OptTop
	scene.AddActor OptBot
	Options_OnOptChg
	OptionDMD.LockRenderThread
	OptionDMD.Stage.AddActor scene
	OptionDMD.UnlockRenderThread
	OptionDMDFlasher.Visible = True
End Sub

Sub Options_UpdateDMD
	If OptionDMD is Nothing Then Exit Sub
	Dim DMDp: DMDp = OptionDMD.DmdPixels
	If Not IsEmpty(DMDp) Then
		DMDWidth = OptionDMD.Width
		DMDHeight = OptionDMD.Height
		DMDPixels = DMDp
	End If
End Sub

Sub Options_Close
	bInOptions = False
	OptionDMDFlasher.Visible = False
	If OptionDMD is Nothing Then Exit Sub
	OptionDMD.Run = False
	Set OptionDMD = Nothing
End Sub

Function Options_OnOffText(opt)
	If opt Then
		Options_OnOffText = "ON"
	Else
		Options_OnOffText = "OFF"
	End If
End Function

Sub Options_OnOptChg
	If OptionDMD is Nothing Then Exit Sub
	OptionDMD.LockRenderThread
	If RenderingMode <> 2 Then
		If OptPos < Opt_VRRoomChoice Then
			OptN.Text = (OptPos+1) & "/" & (NOptions - 4)
		Else
			OptN.Text = (OptPos+1 - 4) & "/" & (NOptions - 4)
		End If
	Else
		OptN.Text = (OptPos+1) & "/" & NOptions
	End If
	If OptSelected Then
		OptTop.Font = OptFontLo
		OptBot.Font = OptFontHi
		OptSel.Visible = True
	Else
		OptTop.Font = OptFontHi
		OptBot.Font = OptFontLo
		OptSel.Visible = False
	End If
	If OptPos = Opt_Light Then
		OptTop.Text = "LIGHT LEVEL"
		OptBot.Text = "LEVEL " & LightLevel
		SaveValue cGameName, "LIGHT", LightLevel
	ElseIf OptPos = Opt_LUT Then
		OptTop.Text = "COLOR SATURATION"
'		OptBot.Text = "LUT " & CInt(ColorLUT)
		if ColorLUT = 1 Then OptBot.text = "DISABLED"
		if ColorLUT = 2 Then OptBot.text = "DESATURATED -10%"
		if ColorLUT = 3 Then OptBot.text = "DESATURATED -20%"
		if ColorLUT = 4 Then OptBot.text = "DESATURATED -30%"
		if ColorLUT = 5 Then OptBot.text = "DESATURATED -40%"
		if ColorLUT = 6 Then OptBot.text = "DESATURATED -50%"
		if ColorLUT = 7 Then OptBot.text = "DESATURATED -60%"
		if ColorLUT = 8 Then OptBot.text = "DESATURATED -70%"
		if ColorLUT = 9 Then OptBot.text = "DESATURATED -80%"
		if ColorLUT = 10 Then OptBot.text = "DESATURATED -90%"
		if ColorLUT = 11 Then OptBot.text = "BLACK'N WHITE"
		SaveValue cGameName, "LUT", ColorLUT
	ElseIf OptPos = Opt_Outpost Then
		OptTop.Text = "OUT POST DIFFICULTY"
		If OutPostMod = 0 Then
			OptBot.Text = "EASY"
		ElseIf OutPostMod = 1 Then
			OptBot.Text = "MEDIUM"
		ElseIf OutPostMod = 2 Then
			OptBot.Text = "HARD"
		ElseIf OutPostMod = 3 Then
			OptBot.Text = "HARDEST"
		End If
		SaveValue cGameName, "OUTPOST", OutPostMod
	ElseIf OptPos = Opt_Volume Then
		OptTop.Text = "MECH VOLUME"
		OptBot.Text = "LEVEL " & CInt(VolumeDial * 100)
		SaveValue cGameName, "VOLUME", VolumeDial
	ElseIf OptPos = Opt_Volume_Ramp Then
		OptTop.Text = "RAMP VOLUME"
		OptBot.Text = "LEVEL " & CInt(RampRollVolume * 100)
		SaveValue cGameName, "RAMPVOLUME", RampRollVolume
	ElseIf OptPos = Opt_Volume_Ball Then
		OptTop.Text = "BALL VOLUME"
		OptBot.Text = "LEVEL " & CInt(BallRollVolume * 100)
		SaveValue cGameName, "BALLVOLUME", BallRollVolume
	ElseIf OptPos = Opt_Cabinet Then
		OptTop.Text = "CABINET MODE"
		OptBot.Text = Options_OnOffText(CabinetMode)
		SaveValue cGameName, "CABINET", CabinetMode
	ElseIf OptPos = Opt_IncandescentGI Then
		OptTop.Text = "INCANDESCENT GI"
		OptBot.Text = Options_OnOffText(IncandescentGI)
		SaveValue cGameName, "INC_GI", IncandescentGI
	ElseIf OptPos = Opt_DynBallShadow Then
		OptTop.Text = "DYN. BALL SHADOWS"
		OptBot.Text = Options_OnOffText(DynamicBallShadowsOn)
		SaveValue cGameName, "DYNBALLSH", DynamicBallShadowsOn
	ElseIf OptPos = Opt_SpotlightShadow Then
		OptTop.Text = "SPOT LIGHT SHADOWS"
		OptBot.Text = Options_OnOffText(SpotlightShadowsOn)
		SaveValue cGameName, "SPOTSH", SpotlightShadowsOn
	ElseIf OptPos = Opt_AmbientBallShadow Then
		OptTop.Text = "AMB. BALL SHADOWS"
		If AmbientBallShadowOn = 0 Then OptBot.Text = "STATIC"
		If AmbientBallShadowOn = 1 Then OptBot.Text = "MOVING"
		If AmbientBallShadowOn = 2 Then OptBot.Text = "FLASHER"
		SaveValue cGameName, "AMBBALLSH", AmbientBallShadowOn
	ElseIf OptPos = Opt_VRRoomChoice Then
		OptTop.Text = "VR ROOM"
		If VRRoomChoice = 0 Then OptBot.Text = "MINIMAL"
		If VRRoomChoice = 1 Then OptBot.Text = "ULTRA"
		SaveValue cGameName, "VRROOM", VRRoomChoice
	ElseIf OptPos = Opt_VRBackglassGI Then
		OptTop.Text = "DYN. BACKGLASS"
		OptBot.Text = Options_OnOffText(VRBackglassGI)
		SaveValue cGameName, "DYNBACKGLASS", VRBackglassGI
	ElseIf OptPos = Opt_VRSideBlades Then
		OptTop.Text = "SIDE BLADES"
		OptBot.Text = Options_OnOffText(VRSideBlades)
		SaveValue cGameName, "VRSBLADES", VRSideBlades
	ElseIf OptPos = Opt_VRTopperOn Then
		OptTop.Text = "TOPPER"
		OptBot.Text = Options_OnOffText(VRTopperOn)
		SaveValue cGameName, "VRTOPPER", VRTopperOn
	ElseIf OptPos = Opt_Info_1 Then
		OptTop.Text = "VPX " & VersionMajor & "." & VersionMinor & "." & VersionRevision
		OptBot.Text = "IRON MAN VAULT EDITION " & TableVersion
	ElseIf OptPos = Opt_Info_2 Then
		OptTop.Text = "RENDER MODE"
		If RenderingMode = 0 Then OptBot.Text = "DEFAULT"
		If RenderingMode = 1 Then OptBot.Text = "STEREO 3D"
		If RenderingMode = 2 Then OptBot.Text = "VR"
	End If
	OptTop.SetAlignedPosition 127, 1, FlexDMD_Align_TopRight
	OptBot.SetAlignedPosition 64, 16, FlexDMD_Align_Center
	OptionDMD.UnlockRenderThread
	UpdateMods
End Sub

Sub Options_Toggle(amount)
	If OptionDMD is Nothing Then Exit Sub
	If OptPos = Opt_Light Then
		LightLevel = LightLevel + amount * 10
		If LightLevel < 0 Then LightLevel = 100
		If LightLevel > 100 Then LightLevel = 0
	ElseIf OptPos = Opt_LUT Then
		ColorLUT = ColorLUT + amount * 1
		If ColorLUT < 1 Then ColorLUT = 11
		If ColorLUT > 11 Then ColorLUT = 1
	ElseIf OptPos = Opt_Outpost Then
		OutPostMod = OutPostMod + amount
		If OutPostMod < 0 Then OutPostMod = 3
		If OutPostMod > 3 Then OutPostMod = 0
	ElseIf OptPos = Opt_Volume Then
		VolumeDial = VolumeDial + amount * 0.1
		If VolumeDial < 0 Then VolumeDial = 1
		If VolumeDial > 1 Then VolumeDial = 0
	ElseIf OptPos = Opt_Volume_Ramp Then
		RampRollVolume = RampRollVolume + amount * 0.1
		If RampRollVolume < 0 Then RampRollVolume = 1
		If RampRollVolume > 1 Then RampRollVolume = 0
	ElseIf OptPos = Opt_Volume_Ball Then
		BallRollVolume = BallRollVolume + amount * 0.1
		If BallRollVolume < 0 Then BallRollVolume = 1
		If BallRollVolume > 1 Then BallRollVolume = 0
	ElseIf OptPos = Opt_Cabinet Then
		CabinetMode = 1 - CabinetMode
	ElseIf OptPos = Opt_IncandescentGI Then
		IncandescentGI = 1 - IncandescentGI
	ElseIf OptPos = Opt_DynBallShadow Then
		DynamicBallShadowsOn = 1 - DynamicBallShadowsOn
	ElseIf OptPos = Opt_SpotlightShadow Then
		SpotlightShadowsOn = 1 - SpotlightShadowsOn
	ElseIf OptPos = Opt_AmbientBallShadow Then
		AmbientBallShadowOn = AmbientBallShadowOn + 1
		If AmbientBallShadowOn > 2 Then AmbientBallShadowOn = 0
	ElseIf OptPos = Opt_VRRoomChoice Then
		VRRoomChoice = 1 - VRRoomChoice
	ElseIf OptPos = Opt_VRBackglassGI Then
		VRBackglassGI = 1 - VRBackglassGI
	ElseIf OptPos = Opt_VRSideBlades Then
		VRSideBlades = 1 - VRSideBlades
	ElseIf OptPos = Opt_VRTopperOn Then
		VRTopperOn = 1 - VRTopperOn
	End If
End Sub

Sub Options_KeyDown(ByVal keycode)
	If OptSelected Then
		If keycode = LeftMagnaSave Then ' Exit / Cancel
			OptSelected = False
		ElseIf keycode = RightMagnaSave Then ' Enter / Select
			OptSelected = False
		ElseIf keycode = LeftFlipperKey Then ' Next / +
			Options_Toggle	-1
		ElseIf keycode = RightFlipperKey Then ' Prev / -
			Options_Toggle	1
		End If
	Else
		If keycode = LeftMagnaSave Then ' Exit / Cancel
			Options_Close
		ElseIf keycode = RightMagnaSave Then ' Enter / Select
			If OptPos < Opt_Info_1 Then OptSelected = True
		ElseIf keycode = LeftFlipperKey Then ' Next / +
			OptPos = OptPos - 1
			If OptPos = Opt_VRTopperOn And RenderingMode <> 2 Then OptPos = OptPos - 1 ' Skip VR option in non VR mode
			If OptPos = Opt_VRSideBlades And RenderingMode <> 2 Then OptPos = OptPos - 1 ' Skip VR option in non VR mode
			If OptPos = Opt_VRBackglassGI And RenderingMode <> 2 Then OptPos = OptPos - 1 ' Skip VR option in non VR mode
			If OptPos = Opt_VRRoomChoice And RenderingMode <> 2 Then OptPos = OptPos - 1 ' Skip VR option in non VR mode
			If OptPos < 0 Then OptPos = NOptions - 1
		ElseIf keycode = RightFlipperKey Then ' Prev / -
			OptPos = OptPos + 1
			If OptPos = Opt_VRRoomChoice And RenderingMode <> 2 Then OptPos = OptPos + 1 ' Skip VR option in non VR mode
			If OptPos = Opt_VRBackglassGI And RenderingMode <> 2 Then OptPos = OptPos + 1 ' Skip VR option in non VR mode
			If OptPos = Opt_VRSideBlades And RenderingMode <> 2 Then OptPos = OptPos + 1 ' Skip VR option in non VR mode
			If OptPos = Opt_VRTopperOn And RenderingMode <> 2 Then OptPos = OptPos + 1 ' Skip VR option in non VR mode
			If OptPos >= NOPtions Then OptPos = 0
		End If
	End If
	Options_OnOptChg
End Sub

Sub Options_Load
	Dim x
    x = LoadValue(cGameName, "LIGHT") : If x <> "" Then LightLevel = CInt(x) Else LightLevel = 50
    x = LoadValue(cGameName, "LUT") : If x <> "" Then ColorLUT = CInt(x) Else ColorLUT = 1
    x = LoadValue(cGameName, "OUTPOST") : If x <> "" Then OutPostMod = CInt(x) Else OutPostMod = 1
    x = LoadValue(cGameName, "VOLUME") : If x <> "" Then VolumeDial = CNCDbl(x) Else VolumeDial = 0.8
    x = LoadValue(cGameName, "RAMPVOLUME") : If x <> "" Then RampRollVolume = CNCDbl(x) Else RampRollVolume = 0.5
    x = LoadValue(cGameName, "BALLVOLUME") : If x <> "" Then BallRollVolume = CNCDbl(x) Else BallRollVolume = 0.5
    x = LoadValue(cGameName, "CABINET") : If x <> "" Then CabinetMode = CInt(x) Else CabinetMode = 0
    x = LoadValue(cGameName, "INC_GI") : If x <> "" Then IncandescentGI = CInt(x) Else IncandescentGI = 0
    x = LoadValue(cGameName, "DYNBALLSH") : If x <> "" Then DynamicBallShadowsOn = CInt(x) Else DynamicBallShadowsOn = 1
    x = LoadValue(cGameName, "SPOTSH") : If x <> "" Then SpotlightShadowsOn = CInt(x) Else SpotlightShadowsOn = 1
    x = LoadValue(cGameName, "AMBBALLSH") : If x <> "" Then AmbientBallShadowOn = CInt(x) Else AmbientBallShadowOn = 1
    x = LoadValue(cGameName, "VRROOM") : If x <> "" Then VRRoomChoice = CInt(x) Else VRRoomChoice = 0
    x = LoadValue(cGameName, "DYNBACKGLASS") : If x <> "" Then VRBackglassGI = CInt(x) Else VRBackglassGI = 0
    x = LoadValue(cGameName, "VRSBLADES") : If x <> "" Then VRSideBlades = CInt(x) Else VRSideBlades = 0
    x = LoadValue(cGameName, "VRTOPPER") : If x <> "" Then VRTopperOn = CInt(x) Else VRTopperOn = 1
	UpdateMods
End Sub

'----------GI rainbow by iaakki---------
Const GIRainbow = False

if GIRainbow Then
	GIRGB.enabled = True
end If

dim giRed, giGreen, GIBlue
dim giRedDir, giGreenDir, GIBlueDir
dim RGBSpeed

RGBSpeed = 1


giRed = 255
giGreen = 127
giBlue = 0
giRedDir = -1
giGreenDir = 1
giBlueDir = 1




sub GIRGB_timer
	If giRed < 1 then giRed = 1:giRedDir = 1 end If
	If giRed > 254 then giRed = 254:giRedDir = -1 End If

	If giGreen < 1 then giGreen = 1:giGreenDir = 1 end If
	If giGreen > 254 then giGreen = 254:giGreenDir = -1 End If

	If giBlue < 1 then giBlue = 1:giBlueDir = 1 end If
	If giBlue > 254 then giBlue = 254:giBlueDir = -1 End If

	giRed = giRed + giRedDir * RGBSpeed
	giGreen = giGreen + giGreenDir * RGBSpeed
	giBlue = giBlue + giBlueDir * RGBSpeed

	SetGiRGBColor

end sub

sub SetGiRGBColor
	dim c, x
	'*********************
	' GI RGB lighting
	'*********************
	c = RGB(giRed,giGreen,GIBlue)

	For Each x in GILights:x.color = c:Next
	Layer1_LM_GI.Color = c
	Layer2_LM_GI.Color = c
	Parts_LM_GI.Color = c
	Bulb_left_LM_GI.Color = c
	Bulb_right_LM_GI.Color = c
	Bumpers_001_LM_GI.Color = c
	Bumpers_002_LM_GI.Color = c
	Bumpers_003_LM_GI.Color = c
	Gate01_LM_GI.Color = c
	Gate_SpinLeft_LM_GI.Color = c
	Gate_SpinMid_LM_GI.Color = c
	Gate_SpinRight_LM_GI.Color = c
	LFLogo_LM_GI.Color = c
	LSling1_LM_GI.Color = c
	LSling2_LM_GI.Color = c
	Orbitpole_01_LM_GI.Color = c
	Orbitpole_Top_LM_GI.Color = c
	RFLogo_LM_GI.Color = c
	RSling1_LM_GI.Color = c
	RSling2_LM_GI.Color = c
	Sling_Arms_LM_GI.Color = c
	Sling_Arms_001_LM_GI.Color = c
	gate02_LM_GI.Color = c
	gate03_LM_GI.Color = c
	gate04_LM_GI.Color = c
	gate05_LM_GI.Color = c
	loutpost_LM_GI.Color = c
	routpost_LM_GI.Color = c
	sw24_LM_GI.Color = c
	sw25_LM_GI.Color = c
	sw28_LM_GI.Color = c
	sw29_LM_GI.Color = c
	sw33_LM_GI.Color = c
	sw34_LM_GI.Color = c
	sw35_LM_GI.Color = c
	sw36_LM_GI.Color = c
	sw38_LM_GI.Color = c
	sw39_LM_GI.Color = c
	sw40_LM_GI.Color = c
	sw41_LM_GI.Color = c
	sw42_LM_GI.Color = c
	sw44_LM_GI.Color = c
	sw45_LM_GI.Color = c
	sw46_LM_GI.Color = c
	sw47_LM_GI.Color = c
	sw48_LM_GI.Color = c
	sw50_LM_GI.Color = c
	sw7_LM_GI.Color = c
	sw9_LM_GI.Color = c
	IMTop_LM_GI.Color = c
	IMTopUpped_LM_GI.Color = c
	IMBot_LM_GI.Color = c
	Parts_LM_GI_001.Color = c
	Parts_LM_GI_002.Color = c
	

end Sub

'----------end GI rainbow by iaakki---------

Sub UpdateMods
	Dim x, y, c, enabled

	'*********************
	' Incandescent/LED lighting
	'*********************
	
	If IncandescentGI Then
		c = RGB(255,169,87) ' 2700K from https://andi-siess.de/rgb-to-color-temperature/
	Else
		c = RGB(255,255,255)
	End If
	' Sadly, as of 0.0.5.1, the toolkit does not manages this automatically
	For Each x in GILights:x.color = c:Next
	Layer1_LM_GI.Color = c
	Layer2_LM_GI.Color = c
	Parts_LM_GI.Color = c
	Bulb_left_LM_GI.Color = c
	Bulb_right_LM_GI.Color = c
	Bumpers_001_LM_GI.Color = c
	Bumpers_002_LM_GI.Color = c
	Bumpers_003_LM_GI.Color = c
	Gate01_LM_GI.Color = c
	Gate_SpinLeft_LM_GI.Color = c
	Gate_SpinMid_LM_GI.Color = c
	Gate_SpinRight_LM_GI.Color = c
	LFLogo_LM_GI.Color = c
	LSling1_LM_GI.Color = c
	LSling2_LM_GI.Color = c
	Orbitpole_01_LM_GI.Color = c
	Orbitpole_Top_LM_GI.Color = c
	RFLogo_LM_GI.Color = c
	RSling1_LM_GI.Color = c
	RSling2_LM_GI.Color = c
	Sling_Arms_LM_GI.Color = c
	Sling_Arms_001_LM_GI.Color = c
	gate02_LM_GI.Color = c
	gate03_LM_GI.Color = c
	gate04_LM_GI.Color = c
	gate05_LM_GI.Color = c
	loutpost_LM_GI.Color = c
	routpost_LM_GI.Color = c
	sw24_LM_GI.Color = c
	sw25_LM_GI.Color = c
	sw28_LM_GI.Color = c
	sw29_LM_GI.Color = c
	sw33_LM_GI.Color = c
	sw34_LM_GI.Color = c
	sw35_LM_GI.Color = c
	sw36_LM_GI.Color = c
	sw38_LM_GI.Color = c
	sw39_LM_GI.Color = c
	sw40_LM_GI.Color = c
	sw41_LM_GI.Color = c
	sw42_LM_GI.Color = c
	sw44_LM_GI.Color = c
	sw45_LM_GI.Color = c
	sw46_LM_GI.Color = c
	sw47_LM_GI.Color = c
	sw48_LM_GI.Color = c
	sw50_LM_GI.Color = c
	sw7_LM_GI.Color = c
	sw9_LM_GI.Color = c
	IMTop_LM_GI.Color = c
	IMTopUpped_LM_GI.Color = c
	IMBot_LM_GI.Color = c
	Parts_LM_GI_001.Color = c
	Parts_LM_GI_002.Color = c
	
	'*********************
	'VR Topper
	'*********************
	
	If VRTopperOn = 1 then
		For each Stuff in VR_Topper: Stuff.visible = true: Next
		VRSuitTimer.enabled = true
	Else
		For each Stuff in VR_Topper: Stuff.visible = False: Next
		VRSuitTimer.enabled = False
	End If
	
	'*********************
	'VR Side blades
	'*********************

	If VRSideBlades = 1 Then
		PinCab_Blades.visible = 1
	Else
		PinCab_Blades.visible = 0
	End If

	'*********************
	'VR Room
	'*********************
	
	If RenderingMode = 2 Then VRRoom = VRRoomChoice + 1 Else VRRoom = 0

	DIM VRThings
	ScoreText.visible = false
	If VRRoom > 0 Then
		DMD.visible = 1
		Setbackglass
		If VRBackglassGI = 0 Then
			For each VRthings in VRBGGI: VRthings.visible = 1 : Next
		End If
		If VRRoom = 1 Then
			for each VRThings in VR_Min:VRThings.visible = 1:Next
		End If
		If VRRoom = 2 Then
			for each VRThings in VR_Min:VRThings.visible = 0:Next
			PinCab_Backbox.visible = 1
			bgdark.visible = 1
		End If
	Else
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		if DesktopMode then 
			PinCab_Rails.visible = true 
			ScoreText.visible = true
		Else 
			PinCab_Rails.visible = false
		End If
	End if

	'*********************
	'Cabinet Mode
	'*********************

	If CabinetMode = 1 and VRRoom < 1 then
		PinCab_Rails.visible=0
	Else
		PinCab_Rails.visible=1
	end If
	
	'*********************
	'Room light level
	'*********************

	Lampz.state(150) = LightLevel
    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial PinCab_Rails.material, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    UpdateMaterial PinCab_Rails.material, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, rgb(64 + (128 * LightLevel) / 100,0,0), glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle

	'*********************
	'Color LUT
	'*********************

	if ColorLUT = 1 Then Table.ColorGradeImage = ""
	if ColorLUT = 2 Then Table.ColorGradeImage = "colorgradelut256x16-10"
	if ColorLUT = 3 Then Table.ColorGradeImage = "colorgradelut256x16-20"
	if ColorLUT = 4 Then Table.ColorGradeImage = "colorgradelut256x16-30"
	if ColorLUT = 5 Then Table.ColorGradeImage = "colorgradelut256x16-40"
	if ColorLUT = 6 Then Table.ColorGradeImage = "colorgradelut256x16-50"
	if ColorLUT = 7 Then Table.ColorGradeImage = "colorgradelut256x16-60"
	if ColorLUT = 8 Then Table.ColorGradeImage = "colorgradelut256x16-70"
	if ColorLUT = 9 Then Table.ColorGradeImage = "colorgradelut256x16-80"
	if ColorLUT = 10 Then Table.ColorGradeImage = "colorgradelut256x16-90"
	if ColorLUT = 11 Then Table.ColorGradeImage = "colorgradelut256x16-100"

	'*********************
	'Other mods
	'*********************
	
	If OutPostMod = 0 Then ' Easy
		routpost_BM_Dark_Room.x = 817.4415
		routpost_BM_Dark_Room.y = 1469.287
		loutpost_BM_Dark_Room.x = 51.87117
		loutpost_BM_Dark_Room.y = 1469.451
		rout_easy.collidable = true
		rout_medium.collidable = false
		rout_hard.collidable = false
		rout_hardest.collidable = false
		lout_easy.collidable = true
		lout_medium.collidable = false
		lout_hard.collidable = false
		lout_hardest.collidable = false
	ElseIf OutPostMod = 1 Then ' Medium
		routpost_BM_Dark_Room.x = 823.0609
		routpost_BM_Dark_Room.y = 1459.596
		loutpost_BM_Dark_Room.x = 48.93166
		loutpost_BM_Dark_Room.y = 1457.558
		rout_easy.collidable = false
		rout_medium.collidable = true
		rout_hard.collidable = false
		rout_hardest.collidable = false
		lout_easy.collidable = false
		lout_medium.collidable = true
		lout_hard.collidable = false
		lout_hardest.collidable = false
	ElseIf OutPostMod = 2 Then ' Hard
		routpost_BM_Dark_Room.x = 829.0158
		routpost_BM_Dark_Room.y = 1449.152
		loutpost_BM_Dark_Room.x = 45.53271
		loutpost_BM_Dark_Room.y = 1446.529
		rout_easy.collidable = false
		rout_medium.collidable = false
		rout_hard.collidable = true
		rout_hardest.collidable = false
		lout_easy.collidable = false
		lout_medium.collidable = false
		lout_hard.collidable = true
		lout_hardest.collidable = false
	ElseIf OutPostMod = 3 Then ' Hardest
		routpost_BM_Dark_Room.x = 834.4323
		routpost_BM_Dark_Room.y = 1439.318
		loutpost_BM_Dark_Room.x = 42.86211
		loutpost_BM_Dark_Room.y = 1435.624
		rout_easy.collidable = false
		rout_medium.collidable = false
		rout_hard.collidable = false
		rout_hardest.collidable = true
		lout_easy.collidable = false
		lout_medium.collidable = false
		lout_hard.collidable = false
		lout_hardest.collidable = true
	End If

	x = routpost_BM_Dark_Room.x
	y = routpost_BM_Dark_Room.y
	routpost_BM_Dark_Room.x = x ' VLM.Props;BM;1;routpost
	routpost_LM_Lit_Room.x = x ' VLM.Props;LM;1;routpost
	routpost_LM_GI.x = x ' VLM.Props;LM;1;routpost
	routpost_LM_flashers_l128.x = x ' VLM.Props;LM;1;routpost
	routpost_LM_flashers_l130.x = x ' VLM.Props;LM;1;routpost
	routpost_LM_flashers_l131.x = x ' VLM.Props;LM;1;routpost
	routpost_LM_flashers_l132.x = x ' VLM.Props;LM;1;routpost
	routpost_BM_Dark_Room.y = y ' VLM.Props;BM;2;routpost
	routpost_LM_Lit_Room.y = y ' VLM.Props;LM;2;routpost
	routpost_LM_GI.y = y ' VLM.Props;LM;2;routpost
	routpost_LM_flashers_l128.y = y ' VLM.Props;LM;2;routpost
	routpost_LM_flashers_l130.y = y ' VLM.Props;LM;2;routpost
	routpost_LM_flashers_l131.y = y ' VLM.Props;LM;2;routpost
	routpost_LM_flashers_l132.y = y ' VLM.Props;LM;2;routpost

	x = loutpost_BM_Dark_Room.x
	y = loutpost_BM_Dark_Room.y
	loutpost_BM_Dark_Room.x = x ' VLM.Props;BM;1;loutpost
	loutpost_LM_Lit_Room.x = x ' VLM.Props;LM;1;loutpost
	loutpost_LM_GI.x = x ' VLM.Props;LM;1;loutpost
	loutpost_LM_flashers_l130.x = x ' VLM.Props;LM;1;loutpost
	loutpost_LM_flashers_l131.x = x ' VLM.Props;LM;1;loutpost
	loutpost_BM_Dark_Room.y = y ' VLM.Props;BM;2;loutpost
	loutpost_LM_Lit_Room.y = y ' VLM.Props;LM;2;loutpost
	loutpost_LM_GI.y = y ' VLM.Props;LM;2;loutpost
	loutpost_LM_flashers_l130.y = y ' VLM.Props;LM;2;loutpost
	loutpost_LM_flashers_l131.y = y ' VLM.Props;LM;2;loutpost

End Sub

' Culture neutral string to double conversion (handles situation where you don't know how the string was written)
Function CNCDbl(str)
    Dim strt, Sep, i
    If IsNumeric(str) Then
        CNCDbl = CDbl(str)
    Else
        Sep = Mid(CStr(0.5), 2, 1)
        Select Case Sep
        Case "."
            i = InStr(1, str, ",")
        Case ","
            i = InStr(1, str, ".")
        End Select
        If i = 0 Then     
            CNCDbl = Empty
        Else
            strt = Mid(str, 1, i - 1) & Sep & Mid(str, i + 1)
            If IsNumeric(strt) Then
                CNCDbl = CDbl(strt)
            Else
                CNCDbl = Empty
            End If
        End If
    End If
End Function

Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x 
		obj.height = - obj.y + 350
		obj.y = -70 'adjusts the distance from the backglass towards the user
		obj.rotx=-86.5
	Next
End Sub

'*********************
'VR TOPPER
'*********************

Dim Stuff
Dim ChestSpeed
ChestSpeed = 50
Dim ChestSpeed2
ChestSpeed = 2
Dim TopperSpeed
TopperSpeed = 0.01

TopperGlowbandFront.blenddisablelighting = 12
TopperGlowbandRight.blenddisablelighting = 12
TopperGlowbandLeft.blenddisablelighting = 12
TopperGlowbandRightBack.blenddisablelighting = 12
TopperGlowbandLeftBack.blenddisablelighting = 12
TopperGlowbandBack.blenddisablelighting = 12

Sub VRSuitTimer_timer()
	VRTopperLabel2.opacity = VRTopperLabel2.opacity +ChestSpeed2*4
	VRSuitChestGlow.opacity = VRSuitChestGlow.opacity +ChestSpeed2
	VRTopperBackgroundGlow.opacity = VRTopperBackgroundGlow.opacity +ChestSpeed2/3.5
	VRSuitChest.opacity = VRSuitChest.opacity +ChestSpeed

	if VRSuitChest.opacity = 3000 then ChestSpeed2 = -2 : ChestSpeed = -50: TopperSpeed = -0.1
	if VRSuitChest.opacity = 100 then ChestSpeed2 = 2: ChestSpeed = 50: TopperSpeed = 0.1

	VRSuitChest.rotz = VRSuitChest.rotz+100
	TopperGlowbandFront.blenddisablelighting = TopperGlowbandFront.blenddisablelighting + Topperspeed
	TopperGlowbandRight.blenddisablelighting = TopperGlowbandRight.blenddisablelighting + Topperspeed
	TopperGlowbandLeft.blenddisablelighting = TopperGlowbandLeft.blenddisablelighting + Topperspeed
	TopperGlowbandRightBack.blenddisablelighting = TopperGlowbandRightBack.blenddisablelighting + Topperspeed
	TopperGlowbandLeftBack.blenddisablelighting = TopperGlowbandLeftBack.blenddisablelighting + Topperspeed
	TopperGlowbandBack.blenddisablelighting = TopperGlowbandBack.blenddisablelighting + Topperspeed
End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

TimerVRPlunger2.enabled = true   '  This sits outside of a sub, and tells the timer2 to be enabled at table load

Sub TimerVRPlunger_Timer
if VR_Plunger.Y < 1530 then VR_Plunger.Y = VR_Plunger.y +5  'If the plunger is not fully extend it, then extend it by 5 coordinates in the Y, 
End Sub

Sub TimerVRPlunger2_Timer
VR_Plunger.Y = 0 + (5* Plunger.Position) -20  ' This follows our dummy plunger position for analog plunger hardware users.
end sub


'***********************************
'	END OF OPTIONS
'***********************************

Sub Update_Wires(wire, pushed)
	Dim z : If pushed Then z = -14 Else z = 0
	Select Case wire
		Case 7
	sw7_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw7
	sw7_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw7
	sw7_LM_GI.transz = z ' VLM.Props;LM;1;sw7
	sw7_LM_flashers_l127.transz = z ' VLM.Props;LM;1;sw7
	sw7_LM_flashers_l121.transz = z ' VLM.Props;LM;1;sw7
		Case 9
	sw9_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw9
	sw9_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw9
	sw9_LM_GI.transz = z ' VLM.Props;LM;1;sw9
	sw9_LM_flashers_l120.transz = z ' VLM.Props;LM;1;sw9
		Case 24
	sw24_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw24
	sw24_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw24
	sw24_LM_GI.transz = z ' VLM.Props;LM;1;sw24
	sw24_LM_flashers_l130.transz = z ' VLM.Props;LM;1;sw24
	sw24_LM_flashers_l131.transz = z ' VLM.Props;LM;1;sw24
		Case 25
	sw25_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw25
	sw25_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_GI.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_flashers_l128.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_flashers_l129.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_flashers_l130.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_flashers_l126.transz = z ' VLM.Props;LM;1;sw25
		Case 28
	sw28_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw28
	sw28_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw28
	sw28_LM_GI.transz = z ' VLM.Props;LM;1;sw28
	sw28_LM_flashers_l128.transz = z ' VLM.Props;LM;1;sw28
	sw28_LM_flashers_l130.transz = z ' VLM.Props;LM;1;sw28
	sw28_LM_inserts_l55.transz = z ' VLM.Props;LM;1;sw28
		Case 29
	sw29_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw29
	sw29_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw29
	sw29_LM_GI.transz = z ' VLM.Props;LM;1;sw29
	sw29_LM_flashers_l130.transz = z ' VLM.Props;LM;1;sw29
	sw29_LM_flashers_l132.transz = z ' VLM.Props;LM;1;sw29
		Case 30
	swplunger_BM_Dark_Room.transz = z ' VLM.Props;BM;1;swplunger
	swplunger_LM_Lit_Room.transz = z ' VLM.Props;LM;1;swplunger
		Case 38
	sw38_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw38
	sw38_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_GI.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_flashers_l120.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_flashers_l129.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_inserts_l57.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_inserts_l58.transz = z ' VLM.Props;LM;1;sw38
		Case 39
	sw39_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw39
	sw39_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw39
	sw39_LM_GI.transz = z ' VLM.Props;LM;1;sw39
	sw39_LM_flashers_l120.transz = z ' VLM.Props;LM;1;sw39
	sw39_LM_flashers_l129.transz = z ' VLM.Props;LM;1;sw39
	sw39_LM_inserts_l58.transz = z ' VLM.Props;LM;1;sw39
	sw39_LM_inserts_l61.transz = z ' VLM.Props;LM;1;sw39
	sw39_LM_flashers_l121.transz = z ' VLM.Props;LM;1;sw39
	End Select
End Sub


' ===============================================================
' The following code can be copy/pasted to disable baked lights
' Lights are not removed on export since they are needed for ball
' reflections and may be used for lightmap synchronisation.

Sub HideLightHelper
'	GIRight001.Visible = False
'	L150.Visible = False
	L8.Visible = True
	l10.Visible = True
'	l104.Visible = False
	l11.Visible = True
	l12.Visible = True
	l120.Visible = True		'Pop Bumper Insert Flasher
	l121.Visible = False
	l122.Visible = True		'Warmachine Double Insert Flasher
	l123.Visible = True		'Monger Mid Double Insert Flasher
'	l125.Visible = False		'Monger Under Playfield Flasher
'	l125a.Visible = False		'Monger Under Playfield Flasher
	l126.Visible = False
	l127a.Visible = True	'Warmachine Insert Flasher
	l127b.Visible = False
'	l128.Visible = False		'Iron Monger Chest
	l129a.Visible = False
	l129b.Visible = False
	l13.Visible = True
	l130.Visible = False
	l131.Visible = False
	l132.Visible = False
	l14.Visible = True
	l15.Visible = True
	l16.Visible = True
	l17.Visible = True
	l18.Visible = True
	l19.Visible = True
	l20.Visible = True
	l21.Visible = True
	l22.Visible = True
	l23.Visible = True
	l24.Visible = True
	l25.Visible = True
	l26.Visible = True
	l27.Visible = False		'Shield
	l28.Visible = True
	l29.Visible = True
	l3.Visible = True
	l30.Visible = True
	l31.Visible = True
	l32.Visible = True
	l33.Visible = True
	l34.Visible = True
	l35.Visible = True
	l36.Visible = True
	l37.Visible = True
	l38.Visible = True
	l39.Visible = True
	l4.Visible = False		'Shield
	l40.Visible = True
	l41.Visible = True
	l42.Visible = True
	l43.Visible = True
	l44.Visible = True
	l45.Visible = True
	l46.Visible = True
	l47.Visible = True
	l48.Visible = True
	l49.Visible = True
	l5.Visible = False		'Shield
	l50.Visible = True
	l51.Visible = True
	l52.Visible = True
	l53.Visible = True
	l54.Visible = True
	l55.Visible = False		'ArcReactor
	l56.Visible = True
	l57.Visible = False		'Shield
	l58.Visible = False		'Shield
	l59.Visible = True
	l6.Visible = False		'Shield
	l60.Visible = True
	l61.Visible = True
	l62.Visible = True
	l63.Visible = True
	l7.Visible = False		'Shield
	l9.Visible = True
End Sub

' ===============================================================
' ZVLM       Virtual Pinball X Light Mapper generated code
'
' This file provide default implementation and template to add bakemap
' & lightmap synchronization for position and lighting.
'
' Lines ending with a comment starting with "' VLM." are meant to
' be copy/pasted ONLY ONCE, since the toolkit will take care of
' updating them directly in your table script, each time an
' export is made.


' ===============================================================
' The following code can be copy/pasted to have premade array for
' movable objects:
' - _LM suffixed arrays contains the lightmaps
' - _BM suffixed arrays contains the bakemap
' - _BL suffixed arrays contains both the bakemap & the lightmaps
Dim Bumpers_001_LM: Bumpers_001_LM=Array() ' VLM.Array;LM;Bumpers_001_LM
Dim Bumpers_001_BM: Bumpers_001_BM=Array(Bumpers_001_BM_Dark_Room) ' VLM.Array;BM;Bumpers_001_BM
Dim Bumpers_001_BL: Bumpers_001_BL=Array(Bumpers_001_BM_Dark_Room, Bumpers_001_LM_GI, Bumpers_001_LM_Lit_Room, Bumpers_001_LM_flashers_l120, Bumpers_001_LM_flashers_l123, Bumpers_001_LM_flashers_l129, Bumpers_001_LM_inserts_l57, Bumpers_001_LM_inserts_l58, Bumpers_001_LM_inserts_l9) ' VLM.Array;All;Bumpers_001_BL
Dim Bumpers_002_LM: Bumpers_002_LM=Array() ' VLM.Array;LM;Bumpers_002_LM
Dim Bumpers_002_BM: Bumpers_002_BM=Array(Bumpers_002_BM_Dark_Room) ' VLM.Array;BM;Bumpers_002_BM
Dim Bumpers_002_BL: Bumpers_002_BL=Array(Bumpers_002_BM_Dark_Room, Bumpers_002_LM_GI, Bumpers_002_LM_Lit_Room, Bumpers_002_LM_flashers_l120, Bumpers_002_LM_flashers_l121, Bumpers_002_LM_flashers_l122, Bumpers_002_LM_flashers_l123, Bumpers_002_LM_flashers_l126, Bumpers_002_LM_flashers_l127, Bumpers_002_LM_flashers_l129, Bumpers_002_LM_flashers_l130) ' VLM.Array;All;Bumpers_002_BL
Dim Bumpers_003_LM: Bumpers_003_LM=Array() ' VLM.Array;LM;Bumpers_003_LM
Dim Bumpers_003_BM: Bumpers_003_BM=Array(Bumpers_003_BM_Dark_Room) ' VLM.Array;BM;Bumpers_003_BM
Dim Bumpers_003_BL: Bumpers_003_BL=Array(Bumpers_003_BM_Dark_Room, Bumpers_003_LM_GI, Bumpers_003_LM_Lit_Room, Bumpers_003_LM_flashers_l120, Bumpers_003_LM_flashers_l121, Bumpers_003_LM_flashers_l122, Bumpers_003_LM_flashers_l129, Bumpers_003_LM_flashers_l130, Bumpers_003_LM_inserts_l58) ' VLM.Array;All;Bumpers_003_BL
Dim Gate01_LM: Gate01_LM=Array() ' VLM.Array;LM;Gate01_LM
Dim Gate01_BM: Gate01_BM=Array(Gate01_BM_Dark_Room) ' VLM.Array;BM;Gate01_BM
Dim Gate01_BL: Gate01_BL=Array(Gate01_BM_Dark_Room, Gate01_LM_GI, Gate01_LM_Lit_Room, Gate01_LM_flashers_l127, Gate01_LM_flashers_l131) ' VLM.Array;All;Gate01_BL
Dim Gate_SpinLeft_LM: Gate_SpinLeft_LM=Array() ' VLM.Array;LM;Gate_SpinLeft_LM
Dim Gate_SpinLeft_BM: Gate_SpinLeft_BM=Array(Gate_SpinLeft_BM_Dark_Room) ' VLM.Array;BM;Gate_SpinLeft_BM
Dim Gate_SpinLeft_BL: Gate_SpinLeft_BL=Array(Gate_SpinLeft_BM_Dark_Room, Gate_SpinLeft_LM_GI, Gate_SpinLeft_LM_Lit_Room, Gate_SpinLeft_LM_flashers_l122, Gate_SpinLeft_LM_flashers_l125, Gate_SpinLeft_LM_flashers_l127, Gate_SpinLeft_LM_flashers_l129, Gate_SpinLeft_LM_flashers_l131, Gate_SpinLeft_LM_inserts_l16, Gate_SpinLeft_LM_inserts_l17, Gate_SpinLeft_LM_inserts_l27) ' VLM.Array;All;Gate_SpinLeft_BL
Dim Gate_SpinMid_LM: Gate_SpinMid_LM=Array() ' VLM.Array;LM;Gate_SpinMid_LM
Dim Gate_SpinMid_BM: Gate_SpinMid_BM=Array(Gate_SpinMid_BM_Dark_Room) ' VLM.Array;BM;Gate_SpinMid_BM
Dim Gate_SpinMid_BL: Gate_SpinMid_BL=Array(Gate_SpinMid_BM_Dark_Room, Gate_SpinMid_LM_GI, Gate_SpinMid_LM_Lit_Room, Gate_SpinMid_LM_flashers_l120, Gate_SpinMid_LM_flashers_l121, Gate_SpinMid_LM_flashers_l122, Gate_SpinMid_LM_flashers_l123, Gate_SpinMid_LM_flashers_l126, Gate_SpinMid_LM_flashers_l127, Gate_SpinMid_LM_flashers_l129, Gate_SpinMid_LM_inserts_l10, Gate_SpinMid_LM_inserts_l9) ' VLM.Array;All;Gate_SpinMid_BL
Dim Gate_SpinRight_LM: Gate_SpinRight_LM=Array() ' VLM.Array;LM;Gate_SpinRight_LM
Dim Gate_SpinRight_BM: Gate_SpinRight_BM=Array(Gate_SpinRight_BM_Dark_Room) ' VLM.Array;BM;Gate_SpinRight_BM
Dim Gate_SpinRight_BL: Gate_SpinRight_BL=Array(Gate_SpinRight_BM_Dark_Room, Gate_SpinRight_LM_GI, Gate_SpinRight_LM_Lit_Room, Gate_SpinRight_LM_flashers_l122, Gate_SpinRight_LM_flashers_l129, Gate_SpinRight_LM_flashers_l132, Gate_SpinRight_LM_inserts_l35, Gate_SpinRight_LM_inserts_l37, Gate_SpinRight_LM_inserts_l39, Gate_SpinRight_LM_inserts_l40, Gate_SpinRight_LM_inserts_l41, Gate_SpinRight_LM_inserts_l42, Gate_SpinRight_LM_inserts_l63) ' VLM.Array;All;Gate_SpinRight_BL
Dim IM_001_LM: IM_001_LM=Array() ' VLM.Array;LM;IM_001_LM
Dim IM_001_BM: IM_001_BM=Array(IMBot_BM_Dark_Room) ' VLM.Array;BM;IM_001_BM
Dim IM_001_BL: IM_001_BL=Array(IMBot_BM_Dark_Room, IMBot_LM_GI, IMBot_LM_Lit_Room, IMBot_LM_flashers_l120, IMBot_LM_flashers_l121, IMBot_LM_flashers_l122, IMBot_LM_flashers_l123, IMBot_LM_flashers_l125, IMBot_LM_flashers_l126, IMBot_LM_flashers_l127, IMBot_LM_flashers_l128, IMBot_LM_flashers_l129, IMBot_LM_flashers_l130, IMBot_LM_flashers_l131, IMBot_LM_flashers_l132, IMBot_LM_inserts_l23, IMBot_LM_inserts_l24, IMBot_LM_inserts_l27, IMBot_LM_inserts_l55) ' VLM.Array;All;IM_001_BL
Dim IMPlastics_LM: IMPlastics_LM=Array() ' VLM.Array;LM;IMPlastics_LM
Dim IMPlastics_BM: IMPlastics_BM=Array(IMTop_BM_Dark_Room) ' VLM.Array;BM;IMPlastics_BM
Dim IMPlastics_BL: IMPlastics_BL=Array(IMTop_BM_Dark_Room, IMTop_LM_GI, IMTop_LM_Lit_Room, IMTop_LM_flashers_l120, IMTop_LM_flashers_l121, IMTop_LM_flashers_l122, IMTop_LM_flashers_l123, IMTop_LM_flashers_l126, IMTop_LM_flashers_l127, IMTop_LM_flashers_l129, IMTop_LM_flashers_l131, IMTop_LM_flashers_l132) ' VLM.Array;All;IMPlastics_BL
Dim IMPlastics_003_LM: IMPlastics_003_LM=Array() ' VLM.Array;LM;IMPlastics_003_LM
Dim IMPlastics_003_BM: IMPlastics_003_BM=Array(IMTopUpped_BM_Dark_Room) ' VLM.Array;BM;IMPlastics_003_BM
Dim IMPlastics_003_BL: IMPlastics_003_BL=Array(IMTopUpped_BM_Dark_Room, IMTopUpped_LM_GI, IMTopUpped_LM_Lit_Room, IMTopUpped_LM_flashers_l120, IMTopUpped_LM_flashers_l122, IMTopUpped_LM_flashers_l123, IMTopUpped_LM_flashers_l125, IMTopUpped_LM_flashers_l126, IMTopUpped_LM_flashers_l127, IMTopUpped_LM_flashers_l128, IMTopUpped_LM_flashers_l129, IMTopUpped_LM_flashers_l130, IMTopUpped_LM_flashers_l131, IMTopUpped_LM_flashers_l132, IMTopUpped_LM_inserts_l13, IMTopUpped_LM_inserts_l22, IMTopUpped_LM_inserts_l23, IMTopUpped_LM_inserts_l24, IMTopUpped_LM_inserts_l25, IMTopUpped_LM_inserts_l27) ' VLM.Array;All;IMPlastics_003_BL
Dim LFLogo_LM: LFLogo_LM=Array() ' VLM.Array;LM;LFLogo_LM
Dim LFLogo_BM: LFLogo_BM=Array(LFLogo_BM_Dark_Room) ' VLM.Array;BM;LFLogo_BM
Dim LFLogo_BL: LFLogo_BL=Array(LFLogo_BM_Dark_Room, LFLogo_LM_GI, LFLogo_LM_Lit_Room, LFLogo_LM_flashers_l130, LFLogo_LM_inserts_l3, LFLogo_LM_inserts_l49, LFLogo_LM_inserts_l50, LFLogo_LM_inserts_l51, LFLogo_LM_inserts_l52, LFLogo_LM_inserts_l53, LFLogo_LM_inserts_l55) ' VLM.Array;All;LFLogo_BL
Dim LSling1_LM: LSling1_LM=Array() ' VLM.Array;LM;LSling1_LM
Dim LSling1_BM: LSling1_BM=Array(LSling1_BM_Dark_Room) ' VLM.Array;BM;LSling1_BM
Dim LSling1_BL: LSling1_BL=Array(LSling1_BM_Dark_Room, LSling1_LM_GI, LSling1_LM_flashers_l130) ' VLM.Array;All;LSling1_BL
Dim LSling2_LM: LSling2_LM=Array() ' VLM.Array;LM;LSling2_LM
Dim LSling2_BM: LSling2_BM=Array(LSling2_BM_Dark_Room) ' VLM.Array;BM;LSling2_BM
Dim LSling2_BL: LSling2_BL=Array(LSling2_BM_Dark_Room, LSling2_LM_GI, LSling2_LM_flashers_l130) ' VLM.Array;All;LSling2_BL
Dim Orbitpole_Top_LM: Orbitpole_Top_LM=Array() ' VLM.Array;LM;Orbitpole_Top_LM
Dim Orbitpole_Top_BM: Orbitpole_Top_BM=Array(Orbitpole_Top_BM_Dark_Room) ' VLM.Array;BM;Orbitpole_Top_BM
Dim Orbitpole_Top_BL: Orbitpole_Top_BL=Array(Orbitpole_Top_BM_Dark_Room, Orbitpole_Top_LM_GI, Orbitpole_Top_LM_Lit_Room, Orbitpole_Top_LM_flashers_l120, Orbitpole_Top_LM_flashers_l121, Orbitpole_Top_LM_flashers_l123, Orbitpole_Top_LM_flashers_l126, Orbitpole_Top_LM_flashers_l129, Orbitpole_Top_LM_inserts_l57, Orbitpole_Top_LM_inserts_l60) ' VLM.Array;All;Orbitpole_Top_BL
Dim Orbitpole_01_LM: Orbitpole_01_LM=Array() ' VLM.Array;LM;Orbitpole_01_LM
Dim Orbitpole_01_BM: Orbitpole_01_BM=Array(Orbitpole_01_BM_Dark_Room) ' VLM.Array;BM;Orbitpole_01_BM
Dim Orbitpole_01_BL: Orbitpole_01_BL=Array(Orbitpole_01_BM_Dark_Room, Orbitpole_01_LM_GI, Orbitpole_01_LM_Lit_Room, Orbitpole_01_LM_flashers_l120, Orbitpole_01_LM_flashers_l123, Orbitpole_01_LM_flashers_l127, Orbitpole_01_LM_flashers_l129) ' VLM.Array;All;Orbitpole_01_BL
Dim RFLogo_LM: RFLogo_LM=Array() ' VLM.Array;LM;RFLogo_LM
Dim RFLogo_BM: RFLogo_BM=Array(RFLogo_BM_Dark_Room) ' VLM.Array;BM;RFLogo_BM
Dim RFLogo_BL: RFLogo_BL=Array(RFLogo_BM_Dark_Room, RFLogo_LM_GI, RFLogo_LM_Lit_Room, RFLogo_LM_flashers_l130, RFLogo_LM_inserts_l3, RFLogo_LM_inserts_l49, RFLogo_LM_inserts_l50, RFLogo_LM_inserts_l51, RFLogo_LM_inserts_l52, RFLogo_LM_inserts_l53, RFLogo_LM_inserts_l55) ' VLM.Array;All;RFLogo_BL
Dim RSling1_LM: RSling1_LM=Array() ' VLM.Array;LM;RSling1_LM
Dim RSling1_BM: RSling1_BM=Array(RSling1_BM_Dark_Room) ' VLM.Array;BM;RSling1_BM
Dim RSling1_BL: RSling1_BL=Array(RSling1_BM_Dark_Room, RSling1_LM_GI, RSling1_LM_flashers_l130) ' VLM.Array;All;RSling1_BL
Dim RSling2_LM: RSling2_LM=Array() ' VLM.Array;LM;RSling2_LM
Dim RSling2_BM: RSling2_BM=Array(RSling2_BM_Dark_Room) ' VLM.Array;BM;RSling2_BM
Dim RSling2_BL: RSling2_BL=Array(RSling2_BM_Dark_Room, RSling2_LM_GI, RSling2_LM_flashers_l130) ' VLM.Array;All;RSling2_BL
Dim Sling_Arms_LM: Sling_Arms_LM=Array() ' VLM.Array;LM;Sling_Arms_LM
Dim Sling_Arms_BM: Sling_Arms_BM=Array(Sling_Arms_BM_Dark_Room) ' VLM.Array;BM;Sling_Arms_BM
Dim Sling_Arms_BL: Sling_Arms_BL=Array(Sling_Arms_BM_Dark_Room, Sling_Arms_LM_GI, Sling_Arms_LM_Lit_Room, Sling_Arms_LM_flashers_l128, Sling_Arms_LM_flashers_l130, Sling_Arms_LM_inserts_l55) ' VLM.Array;All;Sling_Arms_BL
Dim Sling_Arms_001_LM: Sling_Arms_001_LM=Array() ' VLM.Array;LM;Sling_Arms_001_LM
Dim Sling_Arms_001_BM: Sling_Arms_001_BM=Array(Sling_Arms_001_BM_Dark_Room) ' VLM.Array;BM;Sling_Arms_001_BM
Dim Sling_Arms_001_BL: Sling_Arms_001_BL=Array(Sling_Arms_001_BM_Dark_Room, Sling_Arms_001_LM_GI, Sling_Arms_001_LM_Lit_Room, Sling_Arms_001_LM_flashers_l128, Sling_Arms_001_LM_flashers_l130) ' VLM.Array;All;Sling_Arms_001_BL
Dim gate02_LM: gate02_LM=Array() ' VLM.Array;LM;gate02_LM
Dim gate02_BM: gate02_BM=Array(gate02_BM_Dark_Room) ' VLM.Array;BM;gate02_BM
Dim gate02_BL: gate02_BL=Array(gate02_BM_Dark_Room, gate02_LM_GI, gate02_LM_Lit_Room, gate02_LM_flashers_l121) ' VLM.Array;All;gate02_BL
Dim gate03_LM: gate03_LM=Array() ' VLM.Array;LM;gate03_LM
Dim gate03_BM: gate03_BM=Array(gate03_BM_Dark_Room) ' VLM.Array;BM;gate03_BM
Dim gate03_BL: gate03_BL=Array(gate03_BM_Dark_Room, gate03_LM_GI, gate03_LM_Lit_Room, gate03_LM_flashers_l120, gate03_LM_flashers_l126, gate03_LM_flashers_l129, gate03_LM_flashers_l130) ' VLM.Array;All;gate03_BL
Dim gate04_LM: gate04_LM=Array() ' VLM.Array;LM;gate04_LM
Dim gate04_BM: gate04_BM=Array(gate04_BM_Dark_Room) ' VLM.Array;BM;gate04_BM
Dim gate04_BL: gate04_BL=Array(gate04_BM_Dark_Room, gate04_LM_GI, gate04_LM_flashers_l120, gate04_LM_flashers_l126) ' VLM.Array;All;gate04_BL
Dim gate05_LM: gate05_LM=Array() ' VLM.Array;LM;gate05_LM
Dim gate05_BM: gate05_BM=Array(gate05_BM_Dark_Room) ' VLM.Array;BM;gate05_BM
Dim gate05_BL: gate05_BL=Array(gate05_BM_Dark_Room, gate05_LM_GI, gate05_LM_flashers_l120, gate05_LM_flashers_l123, gate05_LM_inserts_l9) ' VLM.Array;All;gate05_BL
Dim loutpost_LM: loutpost_LM=Array() ' VLM.Array;LM;loutpost_LM
Dim loutpost_BM: loutpost_BM=Array(loutpost_BM_Dark_Room) ' VLM.Array;BM;loutpost_BM
Dim loutpost_BL: loutpost_BL=Array(loutpost_BM_Dark_Room, loutpost_LM_GI, loutpost_LM_Lit_Room, loutpost_LM_flashers_l130, loutpost_LM_flashers_l131) ' VLM.Array;All;loutpost_BL
Dim routpost_LM: routpost_LM=Array() ' VLM.Array;LM;routpost_LM
Dim routpost_BM: routpost_BM=Array(routpost_BM_Dark_Room) ' VLM.Array;BM;routpost_BM
Dim routpost_BL: routpost_BL=Array(routpost_BM_Dark_Room, routpost_LM_GI, routpost_LM_Lit_Room, routpost_LM_flashers_l128, routpost_LM_flashers_l130, routpost_LM_flashers_l131, routpost_LM_flashers_l132) ' VLM.Array;All;routpost_BL
Dim sw24_LM: sw24_LM=Array() ' VLM.Array;LM;sw24_LM
Dim sw24_BM: sw24_BM=Array(sw24_BM_Dark_Room) ' VLM.Array;BM;sw24_BM
Dim sw24_BL: sw24_BL=Array(sw24_BM_Dark_Room, sw24_LM_GI, sw24_LM_Lit_Room, sw24_LM_flashers_l130, sw24_LM_flashers_l131) ' VLM.Array;All;sw24_BL
Dim sw25_LM: sw25_LM=Array() ' VLM.Array;LM;sw25_LM
Dim sw25_BM: sw25_BM=Array(sw25_BM_Dark_Room) ' VLM.Array;BM;sw25_BM
Dim sw25_BL: sw25_BL=Array(sw25_BM_Dark_Room, sw25_LM_GI, sw25_LM_Lit_Room, sw25_LM_flashers_l126, sw25_LM_flashers_l128, sw25_LM_flashers_l129, sw25_LM_flashers_l130) ' VLM.Array;All;sw25_BL
Dim sw28_LM: sw28_LM=Array() ' VLM.Array;LM;sw28_LM
Dim sw28_BM: sw28_BM=Array(sw28_BM_Dark_Room) ' VLM.Array;BM;sw28_BM
Dim sw28_BL: sw28_BL=Array(sw28_BM_Dark_Room, sw28_LM_GI, sw28_LM_Lit_Room, sw28_LM_flashers_l128, sw28_LM_flashers_l130, sw28_LM_inserts_l55) ' VLM.Array;All;sw28_BL
Dim sw29_LM: sw29_LM=Array() ' VLM.Array;LM;sw29_LM
Dim sw29_BM: sw29_BM=Array(sw29_BM_Dark_Room) ' VLM.Array;BM;sw29_BM
Dim sw29_BL: sw29_BL=Array(sw29_BM_Dark_Room, sw29_LM_GI, sw29_LM_Lit_Room, sw29_LM_flashers_l130, sw29_LM_flashers_l132) ' VLM.Array;All;sw29_BL
Dim sw33_LM: sw33_LM=Array() ' VLM.Array;LM;sw33_LM
Dim sw33_BM: sw33_BM=Array(sw33_BM_Dark_Room) ' VLM.Array;BM;sw33_BM
Dim sw33_BL: sw33_BL=Array(sw33_BM_Dark_Room, sw33_LM_GI, sw33_LM_Lit_Room, sw33_LM_flashers_l122, sw33_LM_flashers_l131, sw33_LM_inserts_l17, sw33_LM_inserts_l18, sw33_LM_inserts_l19) ' VLM.Array;All;sw33_BL
Dim sw34_LM: sw34_LM=Array() ' VLM.Array;LM;sw34_LM
Dim sw34_BM: sw34_BM=Array(sw34_BM_Dark_Room) ' VLM.Array;BM;sw34_BM
Dim sw34_BL: sw34_BL=Array(sw34_BM_Dark_Room, sw34_LM_GI, sw34_LM_Lit_Room, sw34_LM_flashers_l122, sw34_LM_flashers_l131, sw34_LM_inserts_l17, sw34_LM_inserts_l18, sw34_LM_inserts_l19, sw34_LM_inserts_l20, sw34_LM_inserts_l27, sw34_LM_inserts_l33) ' VLM.Array;All;sw34_BL
Dim sw35_LM: sw35_LM=Array() ' VLM.Array;LM;sw35_LM
Dim sw35_BM: sw35_BM=Array(sw35_BM_Dark_Room) ' VLM.Array;BM;sw35_BM
Dim sw35_BL: sw35_BL=Array(sw35_BM_Dark_Room, sw35_LM_GI, sw35_LM_Lit_Room, sw35_LM_flashers_l131, sw35_LM_inserts_l17, sw35_LM_inserts_l18, sw35_LM_inserts_l19, sw35_LM_inserts_l20, sw35_LM_inserts_l30, sw35_LM_inserts_l31, sw35_LM_inserts_l32) ' VLM.Array;All;sw35_BL
Dim sw36_LM: sw36_LM=Array() ' VLM.Array;LM;sw36_LM
Dim sw36_BM: sw36_BM=Array(sw36_BM_Dark_Room) ' VLM.Array;BM;sw36_BM
Dim sw36_BL: sw36_BL=Array(sw36_BM_Dark_Room, sw36_LM_GI, sw36_LM_Lit_Room, sw36_LM_flashers_l130, sw36_LM_flashers_l131, sw36_LM_inserts_l17, sw36_LM_inserts_l18, sw36_LM_inserts_l19, sw36_LM_inserts_l20, sw36_LM_inserts_l55) ' VLM.Array;All;sw36_BL
Dim sw38_LM: sw38_LM=Array() ' VLM.Array;LM;sw38_LM
Dim sw38_BM: sw38_BM=Array(sw38_BM_Dark_Room) ' VLM.Array;BM;sw38_BM
Dim sw38_BL: sw38_BL=Array(sw38_BM_Dark_Room, sw38_LM_GI, sw38_LM_Lit_Room, sw38_LM_flashers_l120, sw38_LM_flashers_l129, sw38_LM_inserts_l57, sw38_LM_inserts_l58) ' VLM.Array;All;sw38_BL
Dim sw39_LM: sw39_LM=Array() ' VLM.Array;LM;sw39_LM
Dim sw39_BM: sw39_BM=Array(sw39_BM_Dark_Room) ' VLM.Array;BM;sw39_BM
Dim sw39_BL: sw39_BL=Array(sw39_BM_Dark_Room, sw39_LM_GI, sw39_LM_Lit_Room, sw39_LM_flashers_l120, sw39_LM_flashers_l121, sw39_LM_flashers_l129, sw39_LM_inserts_l58, sw39_LM_inserts_l61) ' VLM.Array;All;sw39_BL
Dim sw40_LM: sw40_LM=Array() ' VLM.Array;LM;sw40_LM
Dim sw40_BM: sw40_BM=Array(sw40_BM_Dark_Room) ' VLM.Array;BM;sw40_BM
Dim sw40_BL: sw40_BL=Array(sw40_BM_Dark_Room, sw40_LM_GI, sw40_LM_Lit_Room, sw40_LM_flashers_l130, sw40_LM_flashers_l132, sw40_LM_inserts_l30, sw40_LM_inserts_l36, sw40_LM_inserts_l37, sw40_LM_inserts_l38, sw40_LM_inserts_l42, sw40_LM_inserts_l55, sw40_LM_inserts_l6, sw40_LM_inserts_l63) ' VLM.Array;All;sw40_BL
Dim sw41_LM: sw41_LM=Array() ' VLM.Array;LM;sw41_LM
Dim sw41_BM: sw41_BM=Array(sw41_BM_Dark_Room) ' VLM.Array;BM;sw41_BM
Dim sw41_BL: sw41_BL=Array(sw41_BM_Dark_Room, sw41_LM_GI, sw41_LM_Lit_Room, sw41_LM_flashers_l130, sw41_LM_flashers_l132, sw41_LM_inserts_l36, sw41_LM_inserts_l37, sw41_LM_inserts_l38, sw41_LM_inserts_l40, sw41_LM_inserts_l55) ' VLM.Array;All;sw41_BL
Dim sw42_LM: sw42_LM=Array() ' VLM.Array;LM;sw42_LM
Dim sw42_BM: sw42_BM=Array(sw42_BM_Dark_Room) ' VLM.Array;BM;sw42_BM
Dim sw42_BL: sw42_BL=Array(sw42_BM_Dark_Room, sw42_LM_GI, sw42_LM_Lit_Room, sw42_LM_flashers_l130, sw42_LM_flashers_l132, sw42_LM_inserts_l36, sw42_LM_inserts_l37, sw42_LM_inserts_l38, sw42_LM_inserts_l55, sw42_LM_inserts_l6) ' VLM.Array;All;sw42_BL
Dim sw44_LM: sw44_LM=Array() ' VLM.Array;LM;sw44_LM
Dim sw44_BM: sw44_BM=Array(sw44_BM_Dark_Room) ' VLM.Array;BM;sw44_BM
Dim sw44_BL: sw44_BL=Array(sw44_BM_Dark_Room, sw44_LM_GI, sw44_LM_Lit_Room, sw44_LM_flashers_l122, sw44_LM_flashers_l127, sw44_LM_flashers_l130, sw44_LM_inserts_l27) ' VLM.Array;All;sw44_BL
Dim sw45_LM: sw45_LM=Array() ' VLM.Array;LM;sw45_LM
Dim sw45_BM: sw45_BM=Array(sw45_BM_Dark_Room) ' VLM.Array;BM;sw45_BM
Dim sw45_BL: sw45_BL=Array(sw45_BM_Dark_Room, sw45_LM_GI, sw45_LM_Lit_Room, sw45_LM_flashers_l120, sw45_LM_flashers_l122, sw45_LM_flashers_l129, sw45_LM_flashers_l130) ' VLM.Array;All;sw45_BL
Dim sw46_LM: sw46_LM=Array() ' VLM.Array;LM;sw46_LM
Dim sw46_BM: sw46_BM=Array(sw46_BM_Dark_Room) ' VLM.Array;BM;sw46_BM
Dim sw46_BL: sw46_BL=Array(sw46_BM_Dark_Room, sw46_LM_GI, sw46_LM_Lit_Room, sw46_LM_flashers_l120, sw46_LM_flashers_l122, sw46_LM_flashers_l123, sw46_LM_flashers_l129) ' VLM.Array;All;sw46_BL
Dim sw47_LM: sw47_LM=Array() ' VLM.Array;LM;sw47_LM
Dim sw47_BM: sw47_BM=Array(sw47_BM_Dark_Room) ' VLM.Array;BM;sw47_BM
Dim sw47_BL: sw47_BL=Array(sw47_BM_Dark_Room, sw47_LM_GI, sw47_LM_Lit_Room, sw47_LM_flashers_l122, sw47_LM_flashers_l129, sw47_LM_flashers_l130) ' VLM.Array;All;sw47_BL
Dim sw48_LM: sw48_LM=Array() ' VLM.Array;LM;sw48_LM
Dim sw48_BM: sw48_BM=Array(sw48_BM_Dark_Room) ' VLM.Array;BM;sw48_BM
Dim sw48_BL: sw48_BL=Array(sw48_BM_Dark_Room, sw48_LM_GI, sw48_LM_Lit_Room, sw48_LM_flashers_l129, sw48_LM_flashers_l130) ' VLM.Array;All;sw48_BL
Dim sw50_LM: sw50_LM=Array() ' VLM.Array;LM;sw50_LM
Dim sw50_BM: sw50_BM=Array(sw50_BM_Dark_Room) ' VLM.Array;BM;sw50_BM
Dim sw50_BL: sw50_BL=Array(sw50_BM_Dark_Room, sw50_LM_GI, sw50_LM_Lit_Room, sw50_LM_flashers_l130) ' VLM.Array;All;sw50_BL
Dim sw7_LM: sw7_LM=Array() ' VLM.Array;LM;sw7_LM
Dim sw7_BM: sw7_BM=Array(sw7_BM_Dark_Room) ' VLM.Array;BM;sw7_BM
Dim sw7_BL: sw7_BL=Array(sw7_BM_Dark_Room, sw7_LM_GI, sw7_LM_Lit_Room, sw7_LM_flashers_l121, sw7_LM_flashers_l127) ' VLM.Array;All;sw7_BL
Dim sw9_LM: sw9_LM=Array() ' VLM.Array;LM;sw9_LM
Dim sw9_BM: sw9_BM=Array(sw9_BM_Dark_Room) ' VLM.Array;BM;sw9_BM
Dim sw9_BL: sw9_BL=Array(sw9_BM_Dark_Room, sw9_LM_GI, sw9_LM_Lit_Room, sw9_LM_flashers_l120) ' VLM.Array;All;sw9_BL
Dim swplunger_LM: swplunger_LM=Array() ' VLM.Array;LM;swplunger_LM
Dim swplunger_BM: swplunger_BM=Array(swplunger_BM_Dark_Room) ' VLM.Array;BM;swplunger_BM
Dim swplunger_BL: swplunger_BL=Array(swplunger_BM_Dark_Room, swplunger_LM_Lit_Room) ' VLM.Array;All;swplunger_BL

'VPW Version Log;
'0.001 - Sixtoe - New playfield and plastics thanks to bord. Deleted nearly everything, replaced nearly everything, realigned everything else. Still a mess.
'0.002 - Sixtoe - Added new 3D scanned Iron Monger from Dazz, thanks to Flupper for reducing the poly count. Rebuilt and reprofiled ramps. Changed loads more things, tidied up script a bit more, rebuilt the iron monger area and switches, probably loads of other stuff.
'0.003 - Sixtoe - Complete fittings pass, all locknuts, stays and playfield / plastic layer screws added
'0.004 - eMbee - Redrew some plastics
'0.005 - Sixtoe - Added physics, changed table to be longer as ramps went out the end, moved the glass higher, spinner and target textures updated, apron updated (temp fixture), some positioning work, proper pop bumpers installed
'0.006 - Sixtoe - Fixed physics, added correct laneguides, adjusted all fixtures and fittings so most are now correct, redid war machine plastic, started adding inserts for lights, cleaned up assets.
'0.007 - Sixtoe - Added all remaining lighting inserts and halos and named everything, added nfozzy lamps, added and hooked up most flashers (most of which are temporary), adjusted laneguide rubbers
'0.008 - bord - physics tweaks and targets repositioned
'0.009 - Sixtoe - Fixed gate blocking right ramp, more ramp tweaks on both sides, changed iron monger a little including hitbox, physics and it now moves when hit, figured out and added remaining flashers (still placeholders), trimmed GI, changed kicker operation to be more accurate (thanks Bord), added more exaggerated plunger lane ball rest.
'0.010 - apophis - Added Fleep sound package. Added ball playfield rolling and ramp rolling sounds. 
'0.011 - Sixtoe - Added and hooked up modulated flupper flashers, changed bumper cap lighting, added imongerblocker to stop ball moving through him when he's going up, added left ramp rubber end more bouncy (as per real machine), gi tweaked so it's more speedy so it now behaves more like the real machine, replaced whiplash plastic.
'0.012 - Sixtoe - Split whiplash and hooked whips up to his flashers, split imonger and hooked up chest light to flasher, split imongerframe and made rubbers blue, added temp ramp prims from tomate, cut some holes in playfield, tweaked pop flashers 
'0.013 - Sixtoe - Converted to real trough (SAVE THE BALLS!), changed the iron monger lid to the correct texture and adjusted its size, tweaked flashers (still placeholders).
'0.014 - apophis - Changed right flipper start and end angles to be negative. Changed LFPress to RFPress for the right flipper key press calls. 
'0.015 - Sixtoe - Adjusted top left orbit, blacked out holes / black sections of playfield, completely rebuilt flippers, finished setting prliminary colours for inserts, lights & blooms, some playfield object position adjustments, moved centre one way monger entrance gate (Gate2) (was trapping balls previously), adjusted iron monger hit physics to make it less boingy, left ramp end adjusted so ball bounces back up at end like real machine, table in general seems to play pretty close to papa video
'0.016 - Sixtoe - Cut out playfield inserts, cleaned up playfield, added text layer ramp, added outlane difficulty options, replaced wire gate with plate gate in middle gate, 
'0.017 - Sixtoe - Fixed l27, adjusted some other lights, added backwall GI bulb prims, changed GI a bit more, included a table wide GI flasher (LED lighting is weird).
'0.018 - apophis - Drain kicker fortified. Added slingshot corrections. Updated physics scripts to latest. Fixed issue with gBOT definition. Converted BOT to gBOT and removed all GetBalls calls. Added dynamic shadows. Updated ball rolling sounds.
'0.019 - apophis - Added ramp rolling sounds. Added spotlight shadows.
'0.020 - tomate - Full baked added, cleaned some VPX things, added lampz helper code but still dosent work
'0.021 - flux - GI to Lampz tied
'0.022 - tomate - Ramps Separator layer fixed, clear plastics separator layer added, reworked plastics material, new 4k batch baking added
'0.023 - Sixtoe - Removed a lot of now redundant assets from table, hooked up flashers (tuned flashers as blown out), set up ironmonger (lid in the wrong place?), few other fixes
'0.024 - tomate - Added new rendering batch with shorten names 
'0.025 - Sixtoe - Animated iron monger, hooked up sling graphics, added some collections, updated script.
'0.027 - tomate - More fixes in Blender file, new baking batch added
'0.028 - Sixtoe - Imported playfield resilience and fixes that were lost in a fork at some point, tried to fix some script issues.
'0.029 - Sixtoe - Lots of fixes and script cross referenced with x-men to find issues, looks more respectable now.
'0.030 - tomate - Lots af fixes in blender side, new 4k batch added
'0.031 - Sixtoe - Hooked up new primitives and new collections
'0.032 - tomate - light and materials tweaks on blender side, new 4k batch added
'0.033 - Sixtoe - Added dynamiclighting and spotlights for shadows, 
'0.034 - tomate - Plunger ramp reworked, clear plastics, iron monger lighting and inserts tweaked. New 4k batch added
'0.035 - Sixtoe - Tweaked the flipper size, start and end positions and physics, added niwaks option menu.
'0.036 - Sixtoe - Tweaked flippers, inlanes, all posts and rubbers, trough, turned on underplayfield lamps for iron monger, hooked up orbitpole and centrelanepole graphics to pole position, hooked up iron monger flasher overlays to iron monger position.
'0.037 - Sixtoe - flipper hack, needs new models as they can't be dimensionally aligned properly
'0.038 - apophis - Ball brightness (color) calcualted as a function of GI opacity and distance to nearest GI light (per Niwak example but with different equation). Updated ball image. Updated Lampz to only use one -1 interval timer.
'0.039 - flux - Added spinner angle updates to FrameTimer for lightmapper primitives.
'0.040 - tomate - Test render
'0.041 - Sixtoe - Re-added all lamp insert primitives, started to hook them up.
'0.042 - fluffhead35 - Fixing 3d inserts
'0.043 - Sixtoe - Fixed and animated new flipper primitives, added missing flasher inserts, hooked up spinners and orbit posts, hooked up middle gate as placeholder
'0.044 - Tomate - Test Render
'0.045 - Tomate - Test Render
'0.046 - Sixtoe - Added playfield insert text layer, messed about with flasher tracking for iron monger and collections.
'0.047 - Tomate - Test Render
'0.048 - Sixtoe - Tuned inserts, hooked up targets to RothbauerW's standup targets (placeholder prims atm, going to have to add trackers to them all), hooked up ironmonger top, bottom and chest light flasher to collections, disables lid when IM is up, and disables body and flashers when IM is down.
'0.0xx - Not sure what happened here
'0.055 - Sixtoe - Hooked up spinners, Hooked up targets, sorted out some collections, 
'0.056 - Sixtoe - Fixed target animations
'0.057 - Sixtoe - Added VR cabinet with newly aligned primitives and textures.
'0.058 - Sixtoe - Fixed all the VR issues I could find, should be good now.
'0.059 - Sixtoe - Apron wall raised, VR sideblades and options added.
'0.060-61 - Tomate - Test Renders.
'0.062 - Tomate - Fixed L128.
'0.063 - Sixtoe - Fixed some script things, sorted out collections for l128 and l125 to hide certain reflections when monger is down, increased friction of ramps, adjust wall behind whiplash targets, made inlanes more solid, adjusted outlane spinners to be thinner so they don't activate when missing the plates, removed rubber collidables and expanded sw4 and sw6 on iron monger to replicate toy better, messed about with left ramp (still not happy),
'0.064 - apophis - Fixed dynamic shadows so they would show up again.
'0.065 - tomate - L55 and L130 fixed.
'0.066 - Sixtoe - All lights and reflections hooked up to movable objects, changed spinner RotZ to ObjRotZ, redid all insert lighting using VPX lights, hooked up secondary flasher/lights for Iron Monger Purple Centre and Warmachine Cyan inserts, implemented primitive collidable ramps, then took them out again as they didn't change anything and holes were too small.
'0.068 - tomate - New 4k batch added, sw11 and sw14 added
'0.069 - flux - Added Update Wires to update lightmapper prims for rollover switches
'0.070 - tomate - New 4k batch added
'0.071 - tomate - Fixed 4k batch added
'0.072 - Sixtoe - Physics and layout tweaks, replaced vpx ramps with primtive collidable ramps (and adjusted them) and added height walls, 
'0.073 - tomate - Plastic lid tweaked, outlanes post separated, some script changes based on helper.
'0.074 - iaakki - insert top print lights, ball reflections added, GILights ball reflections and assignment to lampz fixed. Nestmap2 alpha mask value to 180 to eliminate jagged edges for inserts, PFOutlanes material adjusted
'0.075 - iaakki - Insert text brightness tied to gi light events, ramp end protectors adjusted, Iron Monger animation hacked to look good in every possible state and lighting condition. Animation code optimized for 16ms timer
'0.076 - apophis - Fixed spinner rotations. Made ball darker in plunger lane and gradually increase in brightness  as it goes up the lane. Automated VRRoom=VRRoomChoice based on RenderingMode. Added metal hit sound to PostOrbit. Added rolling sound on plunger ramp. Made new physics material zCol_LeftRampEnd and applied to left ramp end. 
'0.077 - Sixtoe - Tweaked centre gate, tweaked monger hit threshold, fixed middle spinner, hooked up all primitive gates, fixed orientation and heights of all gates (duh), fixed orbit and center lane posts and hooked up sound to them, hooked up variable difficultly outlane posts to difficultly level, 
'0.078 - Niwak - Fix option menu, add outpost difficulty, fix Lampz lighting setup, rework ironmonger fading, setup for automatic update from the toolkit, fix sling arms with animation, fix bumper ring visual and animations
'0.079 - tomate - New 4k batch added, +10 added to ball brightness control (apophis)
'0.080 - Sixtoe - Fixed some collections, something odd with targets going black when hit, probably need to hook up other movable objects and maybe use the arrays instead for neatness?
'0.081 - iaakki - Spinner shadows added, defaulted to rom version 185 for fast flips support, removed target bouncer from sling bottom corners
'0.082 - Sixtoe - Fixed centre shot gate ball trap, fixed pop up orbit posts visibility on boot and positioning, increased lighting on in/outlane shield emblems, added Rawd's sexy vr topper,
'0.083 - Niwak - Fix option menu, add more toolkit sync, re-add ironmonger fading, add orbit/clane pole animation
'0.084 - Tomate - Reorientated Targets
'0.085 - Sixtoe - Fixed upper playfield ball jail, added orbitpost animation, fixed VR topper metal material (thanks Rawd), added ironmonger ball trap eject (thanks Apophis), 
'0.086 - tomate - New Pf mesh added, targets 45, 46 y 50 fixed in blender side and reimported (still broken in VPX)
'0.087 - Niwak - Fix targets
'0.088 - Sixtoe - Added Iron Monger motor sounds and hit sounds, tweaked whiplash area, enabled collidable on "o" prims for targets so targetbouncer works (duh), added collidable lid walls to whiplash area, changed LiveCatch to 20, 
'0.089 - apophis - Fixed Iron Monger stuck ball case when ball comes from top of PF. Added Narnia catcher (caught ball goes to plunger lane). pRamp_Right x position set to -3. SSR set to OFF.
'0.090 - leojreimroc - Redone VR Backglass Lighting
'0.091 - tomate - spotlights bulbs separated, some other fixes
'0.092 - Sixtoe - Spotlights realigned and hooked up better to GI, gates and spinners realigned, added desktop background image from joepicasso and darkened it, fixed the orbit and centre post animations, tweaked digital tilt, 
'0.093 - Tomate - Changed axis points for ramp gates
'0.094 - Sixtoe - Realigned gates and several other movable items, moved worklog to end of script, added missing light overlays, added momentum randomisers on ramp ends (thanks iaakki), added better ironmonger wobble (thanks apophis), 
'0.095 - Niwak - Add incandescent mod, Adjust rails color to light level, move all options to in game menu, hooked up gi lighting mod to ball reflections, fixed iron monger fading.
'RC1 - Sixtoe - Fixed standup targets so they animate backwards, partially fixed sling animations, minor tweaks
'RC2 - apophis - Added 5 LUTs from Tomate to the options menu. Set ObjRotZ and fixed RotZ on all standup targets, and reverted ST code.
'RC3 - iaakki - ramp exit randomizer maximum reduced. Color Saturation menu items renamed. FlasherPreloader added.
'RC4 - Niwak - add nestmap loading on startup, add full desaturation options, fixed iron monger reverse fading
'v1.0 Release!
