'**************************  V-Pin Workshop Present  **************************
'
' =ccccc,      ,cccc       ccccc      ,cccc,  ?$$$$$$$,  ,ccc,   -ccc        
':::"$$$$bc    $$$$$     ::`$$$$$c,  : $$$$$c`:"$$$$???'`."$$$$c,:`?$$c      
'`::::"?$$$$c,z$$$$F     `:: ?$$$$$c,`:`$$$$$h`:`?$$$,` :::`$$$$$$c,"$$h,    
'  `::::."$$$$$$$$$'    ..,,,:"$$$$$$h, ?$$$$$$c`:"$$$$$$$'::"$$$$$$$$$$$c   
'     `::::"?$$$$$$    :"$$$$c:`$$$$$$$$d$$$P$$$b`:`?$$$c : ::`?$$c "?$$$$h, 
'       `:::.$$$$$$$c,`::`????":`?$$$E"?$$$$h ?$$$.`:?$$$h..,,,:"$$$,:."?$$$c 
'         `: $$$$$$$$$c, ::``  :::"$$$b `"$$$ :"$$$b`:`?$$$$$$$c``?$F `:: ":: 
'          .,$$$$$"?$$$$$c,    `:::"$$$$.::"$.:: ?$$$.:.???????" `:::  ` ``` 
'          'J$$$$P'::"?$$$$h,   `:::`?$$$c`::``:: .:: : :::::''   `          
'         :,$$$$$':::::`?$$$$$c,  ::: "::  ::  ` ::'   ``                    
'        .'J$$$$F  `::::: .::::    ` :::'  `                                 
'       .: ???):     `:: :::::                                               
'       : :::::'        `                                                    
'        ``
'								 Limited Edition
'
'******************************************************************************
'
' X-Men Limited Edition (Wolverine & Magneto) - IPDB No. 5823 / 5824
' © Stern 2012
' https://www.ipdb.org/machine.cgi?id=5823
' https://www.ipdb.org/machine.cgi?id=5824
'
'******************************************************************************
'
'
' V-Pin Workshop Mutants
'******************************************************************************
' Sixtoe - Project Lead, Physical Rebuild, Audio
' Niwak - Blender Toolkit Implementation, Script Tweaks
' Apophis - Iceman Ramp, Kickers, and Script Tweaks
' Iaakki - Magneto magnet helper
' Testing - Rik, BountyBob, PinStratsDan, VPW Team
'
'Based On:
' VPX Mod by Sixtoe, DJRobX, EBIsLit
' VPX Mod by ICPJuggla, HauntFreaks, DJRobX and Arngrim
' based on the original FP conversion by Freneticamnesic.
Option Explicit

'******************************************************************************
'  User Options
' 
'  Most options are only available through the in-game menu which can be
'  displayed by pressing the 2 magna save button at once.
'******************************************************************************

Const SpotlightShadowsOn = 1		'Toggle for shadows from spotlights
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'******************************************************************************

Const TableVersion = "1.0"	     'Table version (also shown in option UI)

Randomize
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
 
Dim DesktopMode: DesktopMode = Table.ShowDT
Dim UseVPMDMD
Const UseVPMColoredDMD = true
If RenderingMode = 2 Then UseVPMDMD = True Else UseVPMDMD = 0' DesktopMode
If Table.ShowFSS = True or DesktopMode = False Then ScoreText.visible = False

'*******************************************
'  Constants and Global Variables
'*******************************************

Const BallSize = 50			'Ball size must be 50
Const BallMass = 1			'Ball mass must be 1
Const tnob = 4				'Total number of balls
Const lob = 0				'Total number of locked balls

Dim tablewidth: tablewidth = Table.width
Dim tableheight: tableheight = Table.height
Const UseVPMModSol = True
 
LoadVPM "01560000", "sam.VBS", 3.10

'********************
'Standard definitions
'********************
 
Const cGameName = "xmn_151h"
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
	VR_Primary_plunger.Y = -46 + (5* Plunger.Position) -20
End Sub

Sub FrameTimer_Timer()
	uppostt
	RLS

	Dim a

	Disc_BM_Dark_Room.RotZ = (Disc_BM_Dark_Room.RotZ + (ttSpinner.Speed/4)) Mod 360
	a = Disc_BM_Dark_Room.RotZ
	Disc_LM_Lit_Room.RotZ = a ' VLM.Props;LM;1;Disc
	Disc_LM_Flashers_f17.RotZ = a ' VLM.Props;LM;1;Disc
	Disc_LM_Flashers_f18.RotZ = a ' VLM.Props;LM;1;Disc
	Disc_LM_Flashers_f19.RotZ = a ' VLM.Props;LM;1;Disc
	Disc_LM_Flashers_f20.RotZ = a ' VLM.Props;LM;1;Disc

	a = RampDiverter.CurrentAngle -180 -77
	Div_BM_Dark_Room.RotZ = a ' VLM.Props;BM;1;Div
	Div_LM_Flashers_f29.RotZ = a ' VLM.Props;LM;1;Div
	Div_LM_Lit_Room.RotZ = a ' VLM.Props;LM;1;Div
	Div_LM_Gi_l103.RotZ = a ' VLM.Props;LM;1;Div
	Div_LM_GI_Red_l101f.RotZ = a ' VLM.Props;LM;1;Div
	Div_LM_Flashers_f22.RotZ = a ' VLM.Props;LM;1;Div

    UpdateFlipperLogo

	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows

	Options_UpdateDMD
End Sub
 
'***********
' Table init.
'***********

'Variables
Dim xx
Dim mTLMag,mTRMag,mDMag,Bump1, Bump2, Bump3, MsLHole
Dim PlungerIM,ttSpinner, DTBank5, DTBank4, DTBank3, mDiverter
Dim XMBall1, XMBall2, XMBall3, XMBall4, gBOT

Sub Table_Init
	Options_Load
	UpdateMods

	vpmInit Me
	With Controller
        .GameName = cGameName
        .SplashInfoLine = "XMEN LE, Stern (2012)" & vbNewLine & "VPW"
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
	Set XMBall1 = sw21.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set XMBall2 = sw20.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set XMBall3 = sw19.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set XMBall4 = sw18.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(XMBall1,XMBall2,XMBall3,XMBall4)
	
	Controller.Switch(18) = 1
	Controller.Switch(19) = 1
	Controller.Switch(20) = 1
	Controller.Switch(21) = 1

    Set mTLMag= New cvpmMagnet
    With mTLMag
        .InitMagnet Magnet1, 16  
        .GrabCenter = False
        .solenoid=51   ' LE
'       .solenoid=32   ' Pro
        .CreateEvents "mTLMag"
    End With
 
    Set mDMag= New cvpmMagnet
    With mDMag
        .InitMagnet Magnet3, 20  
        .GrabCenter = False
		.strength = 20
        .CreateEvents "mDMag"
    End With
 
    Set ttSpinner = New cvpmTurntable
    ttSpinner.InitTurntable TurnTable, 100
    ttSpinner.SpinDown = 10
    ttSpinner.CreateEvents "ttSpinner"

	PinMAMETimer.Enabled = 1
 
'Nudging
    vpmNudge.TiltSwitch=-7
    vpmNudge.Sensitivity=6
    vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot,Bumper1b,Bumper2b,Bumper3b)
 
'Magneto Lock
    lockPin1.Isdropped=1:lockPin2.Isdropped=1

	vpmTimer.AddTimer 1000, "WarmUpDone '"

	InitVpmFFlipsSAM
End Sub

Sub Table_Paused:Controller.Pause = 1:End Sub
Sub Table_unPaused:Controller.Pause = 0:End Sub
Sub Table_exit:Controller.Stop:End sub
 
'*****Keys
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
	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then FlipperActivate RightFlipper1, RFPress1
	End If
	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperRight Then FlipperActivate RightFlipper1, RFPress1
	End If
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
	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then
			FlipperDeActivate RightFlipper1, RFPress1		
		End If
	End If
	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperRight Then 
			FlipperDeActivate RightFlipper1, RFPress1
		End If
	End If
    If keycode = LeftMagnaSave And Not bInOptions Then bOptionsMagna = False
    If keycode = RightMagnaSave And Not bInOptions Then bOptionsMagna = False
	If Keycode = StartGameKey Then Controller.Switch(16) = 0
	If keycode = PlungerKey Then Plunger.Fire : SoundPlungerReleaseBall
	If vpmKeyUp(keycode) Then Exit Sub
End Sub
 
'******************************************
'	Solenoids
'******************************************

SolCallback(1) = "SolRelease"
SolCallback(2) = "solAutofire"
SolCallback(3) = "SolOutLHole"
SolModCallback(4) = "SolMagnetoMagnet"
SolCallback(5) = "SolOutL"
SolCallback(6) = "CLockUp"
SolCallback(7) = "CLockLatch"
'SolModCallback(9)  = "SetLampMod 141," ' FIXME VB These are the bumpers, not the flasher
'SolModCallback(10) = "SetLampMod 142," ' FIXME VB These are the bumpers, not the flasher
'SolModCallback(11) = "SetLampMod 143," ' FIXME VB These are the bumpers, not the flasher
'SolCallback(12) = "SolURFlipper"
SolCallback(15) = "SolLFlipper"
SolCallback(16) = "SolRFlipper"
SolCallback(26) = "vpmSolDiverter RampDiverter,SoundFX(""Diverter"",DOFContactors),"
 
' LE Only
SolCallback(23) = "solDiscMotor"
SolCallback(27) = "solIceManMotor"
'SolCallback(51) = "solWolverineMagnet" ' cvpmMagnet handles this for us.
SolCallback(52) = "solLeftNightcrawler"
SolCallback(53) = "solRightNightcrawler"

SolCallback(57) = "solLeftNightcrawlerLatch"
SolCallback(58) = "solRightNightcrawlerLatch"
 
' PRO only
'SolModCallback(27) = "SetLampMod 127,"
 
' Modulated Flasher and Lights objects
SolModCallback(17) = "SetLampMod 117," ' Left Blue flasher
SolModCallback(18) = "SetLampMod 118," ' Right yellow flasher
SolModCallback(19) = "SetLampMod 119," ' Disc clear flasher
SolModCallback(20) = "SetLampMod 120," ' Disc blue flasher
SolModCallback(21) = "SetLampMod 121," ' Wolverine flasher
SolModCallback(22) = "SetLampMod 122," ' Magneto side flashers
SolModCallback(25) = "SetLampMod 125," ' Pop bumper flasher
SolModCallback(28) = "SetLampMod 128," ' Left Backpanel flasher
SolModCallback(29) = "SetLampMod 129," ' Right Backpanel flasher
SolModCallback(30) = "SetLampMod 130," ' Magneto spot light
SolModCallback(31) = "SetLampMod 131," ' LE Bottom Arch
SolModCallback(32) = "SetLampMod 132," ' Magneto head

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
		RightFlipper1.RotateToEnd
		If RightFlipper.currentangle > RightFlipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
			RandomSoundReflipUpRight RightFlipper1
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If
	Else
		RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SetLampMod(id, val)
	'Debug.print "> " & id & " => " & val
	Lampz.state(id) = val
End Sub


'******************************************
'	Iceman Ramp
'******************************************

Dim IceManMotor:IceManMotor = 0
Dim IceManDir:IceManDir = -1
Dim IceManAngle:IceManAngle = 90
Dim IceManSpeed:IceManSpeed = 0.16
'Dim IceManHold:IceManHold = 0
Dim IcemanFade:IcemanFade = 100
Const IceManMaxAngle = 60
Const IceManMinAngle = 14
TimerIceManMotor.Interval = 8

Dim BallOnIceRamp, IceRampBallLastX, IceRampBallLastY
BallOnIceRamp = Array(False,False,False,False)

Sub TriggerIceRampStart_hit()
	WireRampOff
	WireRampOn False		'On Wire Ramp (Iceman Ramp) Play Wire Ramp Sound
	BallOnIceRamp(activeball.id) = True
End Sub

 
Sub TimerIceManMotor_Timer()
    ' VP doesn't do dynamic collideable objects.   We want to pause the ramp movement if a ball is on the ramp and the ramp is at a destination.  
    'If IceManHold > 0 AND (IceManAngle = IceManMaxAngle OR IceManAngle = IceManMinAngle) Then Exit Sub
    ' Reset just in case is less than 0
    'IceManHold = 0
	dim b, rampdist, vector, vball
    IceManAngle = IceManAngle + IceManDir * IceManSpeed
	IcemanFade = 30.0 + 70.0 * (IceManAngle - IceManMinAngle) / (IceManMaxAngle - IceManMinAngle)
	' Fade left side lightmaps (l41, l42, l43, l52, l59, f25) when the ramp moves away from them
    if IceManAngle > IceManMaxAngle Then
        IceManAngle = IceManMaxAngle
		IceManRampA.Collidable = 1
		IceManRampB.Collidable = 0
        IceManDir = -1
        Controller.Switch(35) = 1
        StopSound "RampMotor"
        me.Enabled = 0
    Elseif IceManAngle < IceManMinAngle Then
        IceManAngle = IceManMinAngle
        IceManRampA.Collidable = 0
        IceManRampB.Collidable = 1
        IceManDir = 1
        Controller.Switch(34) = 1
        StopSound "RampMotor"
        me.Enabled = 0
    Else   
		IceManRampA.Collidable = 0
		IceManRampB.Collidable = 0
        Controller.Switch(34) = 0
        Controller.Switch(35) = 0
		For b = 0 to ubound(gBOT)
			If BallOnIceRamp(b) Then
				rampdist = Distance(TriggerIceRampStart.X,TriggerIceRampStart.Y, gBOT(b).X, gBOT(b).Y)
				vball = BallSpeed(gBOT(b))
				vector = RotPoint(1, 0, IceManAngle+90)
				gBOT(b).X = vector(0)*rampdist + TriggerIceRampStart.X
				gBOT(b).Y = vector(1)*rampdist + TriggerIceRampStart.Y
				gBOT(b).velx = vector(0)*vball
				gBOT(b).vely = vector(1)*vball
			End If
		Next
    End If
    Dim a : a = IceManAngle
	Iceman_BM_Dark_Room.RotZ = a ' VLM.Props;BM;1;IceManRamp.017
	Iceman_LM_Lit_Room.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Gi_l103.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Gi_l103a.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Gi_l103b.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Gi_Blue_l102d.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Gi_Blue_l102.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_GI_Red_l101d.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Gi_White_l100.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Gi_White_l100g.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Inserts_L23.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Inserts_L41.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Inserts_L47.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Inserts_L59.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Inserts_L71.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Inserts_L80.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Flashers_f17.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Flashers_f18.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Flashers_f19.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Flashers_f20.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Flashers_f21.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	Iceman_LM_Flashers_f25.RotZ = a ' VLM.Props;LM;1;IceManRamp.017
	IceManRampShadow.RotZ = 220 + IceManAngle - IceManminAngle
End Sub
 
Sub solIceManMotor(Enabled)
    TimerIceManMotor.Enabled = Enabled
    if Enabled then PlaySoundAtVol SoundFX("RampMotor",DOFGear), TriggerIceRampStart, 0.7
End Sub
 
Sub solMagnetoMagnet(value)
    if Value > 0 then
        mDmag.Strength = 10 * value / 255
        mDMag.MagnetOn = 1
    Else
		mDmag.Strength = 0
        mDMag.MagnetOn = 0
    end If 
end sub


'Magnet Helper to make sure 4 balls can be caught. -iaakki
'- Helper trigger is enabled when ball lock is dropped
dim relBallCount : relBallCount = 0

sub MagnetHelper_hit
	if activeball.vely > 10 And mDMag.MagnetOn = 1 then										'Only counting in balls that are arriving too fast and when magnet is enabled
'		debug.print "speedlim hit: " & activeball.vely & " ball count: " & relBallCount

		MagnetHelper.timerenabled = true													'Starting a timer that will reset the ball counter
		relBallCount = relBallCount + 1														'Counting balls that hit this helper
		mDmag.Strength = mDmag.Strength + (relBallCount * 0.6)								'Increasing magnet strength momentarily if more balls approaching it
		activeball.vely = activeball.vely * 0.3												'Reducing the Y-velocity for balls that are approaching the magnet

'		debug.print "--> speedlim: " & activeball.vely & " mag strength: " & mDmag.Strength
	end if
end sub

sub MagnetHelper_timer
'	debug.print "timer: " & relBallCount

	relBallCount = 0
	MagnetHelper.timerenabled = false
end sub

 
'************************************************************************
'                               Night Crawlers
'************************************************************************
 
Const NightCrawlerVertSpeed = 1.5
Const NightCrawlerShakeSpeed = .2
Const NightCrawlerShakeDecay = .99
Const NightCrawlerHitFactor = .8
Const NightCrawlerMin = 0
Const NightCrawlerMax = 80
 
Dim LeftNightCrawlerState:LeftNightCrawlerState = 0
Const NCStateMoveUp = 1
Const NCStateShake = 2
Const NCStateMoveDownShake = 3
Const NCStateMoveDown = 4
 
Dim RightNightCrawlerState:RightNightCrawlerState = 0
Dim LeftNightCrawlerShake:LeftNightCrawlerShake = 0
Dim RightNightCrawlerShake:RightNightCrawlerShake = 0
Dim LeftNightCrawlerShakeAngle:LeftNightCrawlerShakeAngle = 0
Dim RightNightCrawlerShakeAngle:RightNightCrawlerShakeAngle = 0
 
Sub LeftNightCrawlerWall_Hit
    Controller.Switch(50) = 1
    LeftNightCrawlerShakeAngle = 0
    LeftNightCrawlerShake = BallVel(ActiveBall) * NightCrawlerHitFactor
    LeftNightCrawlerState = NCStateShake
    LeftNightCrawlerWall.TimerEnabled = 1
End Sub
 
Sub LeftNightCrawlerWall_UnHit:Controller.Switch(50) = 0:End Sub
 
Sub RightNightCrawlerWall_Hit
    Controller.Switch(51) = 1
    RightNightCrawlerShakeAngle = 0
    RightNightCrawlerShake = BallVel(ActiveBall) * NightCrawlerHitFactor
    RightNightCrawlerState = NCStateShake
    RightNightCrawlerWall.TimerEnabled = 1
End Sub
 
Sub RightNightCrawlerWall_UnHit:Controller.Switch(51) = 0:End Sub
 
Sub RightNightCrawlerWall_Timer
	Dim y : y = NCR_Bot_BM_Dark_Room.TransY
	Dim z : z = NCR_Bot_BM_Dark_Room.Z
    select case RightNightCrawlerState
    case NCStateMoveDown:
        Z = Z - NightCrawlerVertSpeed
        if Z <= NightCrawlerMin then
            Z = NightCrawlerMin
            RightNightCrawlerWall.IsDropped = 1
            Controller.Switch(56) = 1
            Me.TimerEnabled = 0
        end If
    case NCStateMoveUp:
        Z = Z + NightCrawlerVertSpeed
        if Z >= NightCrawlerMax then
            Z = NightCrawlerMax
            RightNightCrawlerWall.IsDropped = 0
            Controller.Switch(56) = 0
            RightNightCrawlerShakeAngle = 0
            RightNightCrawlerShake = 8
            RightNightCrawlerState = NCStateShake
        end If
    case NCStateShake:
        y = RightNightCrawlerShake * Sin(RightNightCrawlerShakeAngle)
        RightNightCrawlerShakeAngle = RightNightCrawlerShakeAngle + ((NightCrawlerShakeSpeed * RightNightCrawlerShake) / 30)
        RightNightCrawlerShake = RightNightCrawlerShake * NightCrawlerShakeDecay
        if RightNightCrawlerShake < 1 Then 
            y = 0
            Me.TimerEnabled =0
        end If     
    case NCStateMoveDownShake:
        y = RightNightCrawlerShake * Sin(RightNightCrawlerShakeAngle)
        RightNightCrawlerShakeAngle = RightNightCrawlerShakeAngle + ((NightCrawlerShakeSpeed * RightNightCrawlerShake) / 30)
        RightNightCrawlerShake = RightNightCrawlerShake * NightCrawlerShakeDecay
        if RightNightCrawlerShake < 1 Then 
            y = 0
            RightNightCrawlerState = NCStateMoveDown
        end If     
    end Select

	NCR_Bot_BM_Dark_Room.z = z ' VLM.Props;BM;1;NCR.Bot
	NCR_Bot_LM_Lit_Room.z = z ' VLM.Props;LM;1;NCR.Bot
	NCR_Bot_LM_Flashers_f19.z = z ' VLM.Props;LM;1;NCR.Bot
	NCR_Bot_LM_Flashers_f20.z = z ' VLM.Props;LM;1;NCR.Bot
	NCR_Bot_BM_Dark_Room.transy = y ' VLM.Props;BM;2;NCR.Bot
	NCR_Bot_LM_Lit_Room.transy = y ' VLM.Props;LM;2;NCR.Bot
	NCR_Bot_LM_Flashers_f19.transy = y ' VLM.Props;LM;2;NCR.Bot
	NCR_Bot_LM_Flashers_f20.transy = y ' VLM.Props;LM;2;NCR.Bot
	NCR_Top_BM_Dark_Room.z = z ' VLM.Props;BM;1;NCR.Top
	NCR_Top_LM_Lit_Room.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_LM_Gi_l103a.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_LM_Gi_White_l100.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_LM_Flashers_f17.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_LM_Flashers_f18.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_LM_Flashers_f19.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_LM_Flashers_f21.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_LM_Flashers_f22.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_LM_Flashers_f30.z = z ' VLM.Props;LM;1;NCR.Top
	NCR_Top_BM_Dark_Room.transy = y ' VLM.Props;BM;2;NCR.Top
	NCR_Top_LM_Lit_Room.transy = y ' VLM.Props;LM;2;NCR.Top
	NCR_Top_LM_Gi_l103a.transy = y ' VLM.Props;LM;2;NCR.Top
	NCR_Top_LM_Gi_White_l100.transy = y ' VLM.Props;LM;2;NCR.Top
	NCR_Top_LM_Flashers_f17.transy = y ' VLM.Props;LM;2;NCR.Top
	NCR_Top_LM_Flashers_f18.transy = y ' VLM.Props;LM;2;NCR.Top
	NCR_Top_LM_Flashers_f19.transy = y ' VLM.Props;LM;2;NCR.Top
	NCR_Top_LM_Flashers_f21.transy = y ' VLM.Props;LM;2;NCR.Top
	NCR_Top_LM_Flashers_f22.transy = y ' VLM.Props;LM;2;NCR.Top
	NCR_Top_LM_Flashers_f30.transy = y ' VLM.Props;LM;2;NCR.Top
End Sub
 
Sub LeftNightCrawlerWall_Timer
	Dim y : y = NCL_Bot_BM_Dark_Room.TransY
	Dim z : z = NCL_Bot_BM_Dark_Room.Z
    select case LeftNightCrawlerState
    case NCStateMoveDown:
        Z = Z - NightCrawlerVertSpeed
        if Z <= NightCrawlerMin then
            Z = NightCrawlerMin
            LeftNightCrawlerWall.IsDropped = 1
            Controller.Switch(12) = 1
            Me.TimerEnabled = 0
        end If
    case NCStateMoveUp:
        Z = Z + NightCrawlerVertSpeed
        if Z >= NightCrawlerMax then
            Z = NightCrawlerMax
            LeftNightCrawlerWall.IsDropped = 0
            Controller.Switch(12) = 0
            LeftNightCrawlerShakeAngle = 0
            LeftNightCrawlerShake = 8
            LeftNightCrawlerState = NCStateShake
        end If
    case NCStateShake:
        Y = LeftNightCrawlerShake * Sin(LeftNightCrawlerShakeAngle)
        LeftNightCrawlerShakeAngle = LeftNightCrawlerShakeAngle + ((NightCrawlerShakeSpeed * LeftNightCrawlerShake) / 30)
        LeftNightCrawlerShake = LeftNightCrawlerShake * NightCrawlerShakeDecay
        if LeftNightCrawlerShake < 1 Then  
            Y = 0
            Me.TimerEnabled =0
        end If     
    case NCStateMoveDownShake:
        Y = LeftNightCrawlerShake * Sin(LeftNightCrawlerShakeAngle)
        LeftNightCrawlerShakeAngle = LeftNightCrawlerShakeAngle + ((NightCrawlerShakeSpeed * LeftNightCrawlerShake) / 30)
        LeftNightCrawlerShake = LeftNightCrawlerShake * NightCrawlerShakeDecay
        if LeftNightCrawlerShake < 1 Then  
            y = 0
            LeftNightCrawlerState = NCStateMoveDown
        end If     
    end Select

	NCL_Bot_BM_Dark_Room.z = z ' VLM.Props;BM;1;NCL.Bot
	NCL_Bot_LM_Lit_Room.z = z ' VLM.Props;LM;1;NCL.Bot
	NCL_Bot_LM_Inserts_L67.z = z ' VLM.Props;LM;1;NCL.Bot
	NCL_Bot_LM_Flashers_f19.z = z ' VLM.Props;LM;1;NCL.Bot
	NCL_Bot_LM_Flashers_f20.z = z ' VLM.Props;LM;1;NCL.Bot
	NCL_Bot_LM_Flashers_f30.z = z ' VLM.Props;LM;1;NCL.Bot
	NCL_Bot_BM_Dark_Room.transy = y ' VLM.Props;BM;2;NCL.Bot
	NCL_Bot_LM_Lit_Room.transy = y ' VLM.Props;LM;2;NCL.Bot
	NCL_Bot_LM_Inserts_L67.transy = y ' VLM.Props;LM;2;NCL.Bot
	NCL_Bot_LM_Flashers_f19.transy = y ' VLM.Props;LM;2;NCL.Bot
	NCL_Bot_LM_Flashers_f20.transy = y ' VLM.Props;LM;2;NCL.Bot
	NCL_Bot_LM_Flashers_f30.transy = y ' VLM.Props;LM;2;NCL.Bot
	NCL_Top_BM_Dark_Room.z = z ' VLM.Props;BM;1;NCL.Top
	NCL_Top_LM_Lit_Room.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Gi_l103b.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Inserts_L42.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Inserts_L60.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Flashers_f19.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Flashers_f20.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Flashers_f21.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Flashers_f22.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Flashers_f25.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_LM_Flashers_f30.z = z ' VLM.Props;LM;1;NCL.Top
	NCL_Top_BM_Dark_Room.transy = y ' VLM.Props;BM;2;NCL.Top
	NCL_Top_LM_Lit_Room.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Gi_l103b.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Inserts_L42.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Inserts_L60.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Flashers_f19.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Flashers_f20.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Flashers_f21.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Flashers_f22.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Flashers_f25.transy = y ' VLM.Props;LM;2;NCL.Top
	NCL_Top_LM_Flashers_f30.transy = y ' VLM.Props;LM;2;NCL.Top
End Sub
 
Sub SolLeftNightCrawler(Enabled)
    If Enabled AND NCL_Bot_BM_Dark_Room.Z <> NightCrawlerMin then
        LeftNightCrawlerShakeAngle = 0
        LeftNightCrawlerShake = 8
        LeftNightCrawlerState = NCStateMoveDownShake
        LeftNightCrawlerWall.TimerEnabled = 1
        PlaySoundAtVol SoundFX("PopUp",DOFContactors), NCL_Bot_BM_Dark_Room, .001
    End If
End Sub
 
Sub SolLeftNightCrawlerLatch(Enabled)
    If NOT Enabled AND NCL_Bot_BM_Dark_Room.Z = NightCrawlerMin Then
        PlaySoundAtVol SoundFX("solenoid",DOFContactors), NCL_Bot_BM_Dark_Room, .5
        LeftNightCrawlerState = NCStateMoveUp
        LeftNightCrawlerWall.TimerEnabled = 1
		dim b: for each b in gBOT  'kick ball out of way if on top of night crawler
			if InRect(b.x,b.y,373,629,462,629,462,722,373,722) then
				b.velz = 20
				b.vely = b.vely + 5
			end if
		next
    End If
End Sub
 
Sub SolRightNightCrawler(Enabled)
    If Enabled AND NCR_Bot_BM_Dark_Room.Z <> NightCrawlerMin then
        RightNightCrawlerShakeAngle = 0
        RightNightCrawlerShake = 8
        RightNightCrawlerState = NCStateMoveDownShake
        RightNightCrawlerWall.TimerEnabled = 1
        PlaySoundAtVol SoundFX("PopUp",DOFContactors), NCR_Bot_BM_Dark_Room, .001
    End If
End Sub
 
Sub SolRightNightCrawlerLatch(Enabled)
    If NOT Enabled AND NCR_Bot_BM_Dark_Room.Z = NightCrawlerMin Then
        PlaySoundAtVol SoundFX("solenoid",DOFContactors), NCR_Bot_BM_Dark_Room, .5
        RightNightCrawlerState = NCStateMoveUp
        RightNightCrawlerWall.TimerEnabled = 1
		dim b: for each b in gBOT  'kick ball out of way if on top of night crawler
			if InRect(b.x,b.y,653,676,738,707,705,794,622,764) then
				b.velz = 20
				b.vely = b.vely + 5
			end if
		next
    End If
End Sub

LeftNightCrawlerWall_Timer
RightNightCrawlerWall_Timer

'************************************************************************
'                               Turntable
'************************************************************************
 
 
Sub solDiscMotor(Enabled)
    if Enabled Then 
        ttSpinner.MotorOn = True : Playsound SoundFX("disc_noise",DOFGear),-1,0.04,0,0,-60000,0,0,AudioFade(Disc_BM_Dark_Room)
    Else   
        ttSpinner.MotorOn = False : Stopsound "disc_noise"
    end If
end sub
 
Sub TurnTable_Hit
	debug.print activeball.vely
    ttSpinner.AddBall ActiveBall
    if ttSpinner.MotorOn=true then ttSpinner.AffectBall ActiveBall
End Sub
 
Sub TurnTable_unHit
	ttSpinner.RemoveBall ActiveBall
End Sub
 
Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
		SoundPlungerReleaseBall
	End If
End Sub
 
Sub CLockUp(Enabled)
    If Enabled Then
		lockPin1.Isdropped=0:lockPin2.Isdropped=0
		MagnetHelper.enabled = false
		SoundSaucerKick 0,sw53
    End If
 End Sub
 
Sub LockPin1_Timer
    'Dozer would be proud...
    Me.TimerEnabled = 0
    lockPin1.Isdropped=1:lockPin2.Isdropped=1
	MagnetHelper.enabled = true
	SoundSaucerKick 0,sw53
End Sub
 
Sub CLockLatch(Enabled)
    If Enabled Then
         lockPin1.TimerInterval=500
         lockPin1.TimerEnabled=True
    End If
End Sub
 

Sub SolDiverter(enabled)
    If enabled Then
        RampDiverter.rotatetoend : PlaySoundAt SoundFX("Diverter",DOFContactors), l28c
    Else
        RampDiverter.rotatetostart : PlaySoundAt SoundFX("Diverter",DOFContactors), l28c
    End If
End Sub
 

' Kickers
Dim KickerBall4, KickerBall55

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180
    
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub


Sub SolOutLHole(enabled)
    If Enabled then
		If Controller.Switch(4) <> 0 Then
			KickBall KickerBall4, 354, 65, 0, 0
			SoundSaucerKick 1,sw4
			Controller.Switch(4) = 0
		End If
	End If
End Sub

Sub SolOutL(enabled)
    If Enabled then
		If Controller.Switch(55) <> 0 Then
			KickBall KickerBall55, 0, 0, 90, 0
			SoundSaucerKick 1,sw55
			Controller.Switch(55) = 0
		End If
	End If
End Sub

'******************************************
'	Switches
'******************************************

'Sub sw1_Hit:Me.TimerEnabled = 1:sw1p.TransX = -2:vpmTimer.PulseSw 1:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
'Sub sw1_Timer:Me.TimerEnabled = 0:sw1p.TransX = 0:End Sub
'Sub sw2_Hit:Me.TimerEnabled = 1:sw2p.TransX = -2:vpmTimer.PulseSw 2:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
'Sub sw2_Timer:Me.TimerEnabled = 0:sw2p.TransX = 0:End Sub
'Sub sw7_Hit:Me.TimerEnabled = 1:sw7p.TransX = -2:vpmTimer.PulseSw 7:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
'Sub sw7_Timer:Me.TimerEnabled = 0:sw7p.TransX = 0:End Sub
'Sub sw8_Hit:Me.TimerEnabled = 1:sw8p.TransX = -2:vpmTimer.PulseSw 8:PlaySoundAt SoundFX("target",DOFTargets), ActiveBall:End Sub
'Sub sw8_Timer:Me.TimerEnabled = 0:sw8p.TransX = 0:End Sub
Sub sw1_Hit:Me.TimerEnabled = 1:vpmTimer.PulseSw 1:End Sub
Sub sw1_Timer:Me.TimerEnabled = 0:End Sub
Sub sw2_Hit:Me.TimerEnabled = 1:vpmTimer.PulseSw 2:End Sub
Sub sw2_Timer:Me.TimerEnabled = 0:End Sub

Sub sw4_Hit
    set KickerBall4 = activeball
	Controller.Switch(4) = 1
	SoundSaucerLock
End Sub
'Sub sw4_UnHit:Controller.Switch(4) = 0:End Sub

Sub sw7_Hit:Me.TimerEnabled = 1:vpmTimer.PulseSw 7:End Sub
Sub sw7_Timer:Me.TimerEnabled = 0:End Sub
Sub sw8_Hit:Me.TimerEnabled = 1:vpmTimer.PulseSw 8:End Sub
Sub sw8_Timer:Me.TimerEnabled = 0:End Sub
Sub sw11_Hit:vpmTimer.PulseSw 11:End Sub
'Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub
Sub sw13_Hit:vpmTimer.PulseSw 13:End Sub
'Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub
Sub sw14_Hit:  Update_Wires 14, True :Controller.Switch(14) = 1:End Sub
Sub sw14_UnHit:Update_Wires 14, False:Controller.Switch(14) = 0:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1:End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
Sub sw24_Hit:  Update_Wires 24, True :Controller.Switch(24) = 1:End Sub
Sub sw24_UnHit:Update_Wires 24, False:Controller.Switch(24) = 0:End Sub
Sub sw25_Hit:  Update_Wires 25, True :Controller.Switch(25) = 1:activeball.vely = activeball.vely*0.8:activeball.angmomz = 0:End Sub
Sub sw25_UnHit:Update_Wires 25, False:Controller.Switch(25) = 0:End Sub
Sub sw28_Hit:  Update_Wires 28, True :Controller.Switch(28) = 1:activeball.vely = activeball.vely*0.8:activeball.angmomz = 0:End Sub
Sub sw28_UnHit:Update_Wires 28, False:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:  Update_Wires 29, True :Controller.Switch(29) = 1:End Sub
Sub sw29_UnHit:Update_Wires 29, False:Controller.Switch(29) = 0:End Sub
Sub sw33_Hit:  Update_Wires 33, True :Controller.Switch(33) = 1:activeball.vely = activeball.vely*0.8:activeball.angmomz = 0:End Sub
Sub sw33_UnHit:Update_Wires 33, False:Controller.Switch(33) = 0:End Sub
Sub sw36_Hit
	vpmTimer.PulseSw 36
    WobbleValue = cor.ballvel(activeball.id)/10
    WolverineWobble.Enabled = True
End Sub
'Sub sw36_Timer:Wolvie1.IsDropped = 0:Wolvie2.IsDropped = 1:wr1.alpha = 1:wr1.triggersingleupdate:wr2.alpha = 0:wr2.triggersingleupdate:Me.TimerEnabled = 0:End Sub
Sub sw38_Hit:Controller.Switch(38)=1:End Sub     'Lock 2
Sub sw38_unHit:Controller.Switch(38)=0:End Sub
Sub sw39_Hit:Controller.Switch(39)=1:End Sub     'Lock 3
Sub sw39_unHit:Controller.Switch(39)=0:End Sub
Sub sw40_Hit:Controller.Switch(40)=1:End Sub     'Lock 4
Sub sw40_unHit:Controller.Switch(40)=0:End Sub
Sub sw41_Hit:Me.TimerEnabled = 1:sw41p.TransX = -2:vpmTimer.PulseSw 41:End Sub
Sub sw41_Timer:Me.TimerEnabled = 0:sw41p.TransX = 0:End Sub
Sub sw42_Hit:Me.TimerEnabled = 1:sw42p.TransX = -2:vpmTimer.PulseSw 42:End Sub
Sub sw42_Timer:Me.TimerEnabled = 0:sw42p.TransX = 0:End Sub
Sub sw47_Spin:vpmTimer.PulseSw 47:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:End Sub
'Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub
Sub sw49_Hit:  Update_Wires 49, True :Controller.Switch(49) = 1:End Sub
Sub sw49_UnHit:Update_Wires 49, False:Controller.Switch(49) = 0:End Sub
'Sub sw52_Hit:Controller.Switch(52) = 1:IceManHold = IceManHold + 1:End Sub
Sub sw52_Hit:Controller.Switch(52) = 1:End Sub
Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub
Sub sw53_Hit:  Update_Wires 53, True :Controller.Switch(53) = 1:End Sub
Sub sw53_UnHit:Update_Wires 53, False:Controller.Switch(53) = 0:End Sub
Sub sw54_Hit:  Update_Wires 54, True :Controller.Switch(54) = 1:End Sub
Sub sw54_UnHit:Update_Wires 54, False:Controller.Switch(54) = 0:End Sub

Sub sw55_Hit
    set KickerBall55 = activeball
	Controller.Switch(55) = 1
	SoundSaucerLock
End Sub
'Sub sw55_UnHit:Controller.Switch(55) = 0:End Sub

'Sub sw55_Hit:SolOutL.AddBall 1:SoundSaucerLock:End Sub
'Sub sw55_UnHit:End Sub

Sub Update_Wires(wire, pushed)
	Dim z : If pushed Then z = -14 Else z = 0
	Select Case wire
		Case 14
	sw14_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw14
		Case 24
	sw24_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw24
	sw24_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw24
	sw24_LM_Gi_Blue_l102a.transz = z ' VLM.Props;LM;1;sw24
	sw24_LM_GI_Red_l101a.transz = z ' VLM.Props;LM;1;sw24
	sw24_LM_Gi_White_l100b.transz = z ' VLM.Props;LM;1;sw24
	sw24_LM_Flashers_f17.transz = z ' VLM.Props;LM;1;sw24
		Case 25
	sw25_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw25
	sw25_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_Gi_Blue_l102a.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_GI_Red_l101a.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_Gi_White_l100b.transz = z ' VLM.Props;LM;1;sw25
	sw25_LM_Flashers_f17.transz = z ' VLM.Props;LM;1;sw25
		Case 28
		Case 29
	sw29_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw29
	sw29_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw29
	sw29_LM_Gi_Blue_l102b.transz = z ' VLM.Props;LM;1;sw29
	sw29_LM_Flashers_f21.transz = z ' VLM.Props;LM;1;sw29
		Case 33
	sw33_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw33
	sw33_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw33
	sw33_LM_GI_Red_l101b.transz = z ' VLM.Props;LM;1;sw33
	sw33_LM_Flashers_f21.transz = z ' VLM.Props;LM;1;sw33
		Case 49
	sw49_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw49
	sw49_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw49
	sw49_LM_Gi_Blue_l102h.transz = z ' VLM.Props;LM;1;sw49
	sw49_LM_Flashers_f18.transz = z ' VLM.Props;LM;1;sw49
		Case 53
	sw53_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw53
	sw53_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw53
	sw53_LM_Flashers_f18.transz = z ' VLM.Props;LM;1;sw53
	sw53_LM_Flashers_f22.transz = z ' VLM.Props;LM;1;sw53
	sw53_LM_Flashers_f30.transz = z ' VLM.Props;LM;1;sw53
		Case 54
	sw54_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw54
	sw54_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw54
	sw54_LM_Gi_White_l100j.transz = z ' VLM.Props;LM;1;sw54
	sw54_LM_Flashers_f22.transz = z ' VLM.Props;LM;1;sw54
	End Select
End Sub



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
		sw21.kick 60, 9
		RandomSoundBallRelease sw21
	End If
End Sub
'******************************************************
 
Dim LStep : LStep = 4 : LeftSlingShot_Timer
Dim RStep : RStep = 4 : RightSlingShot_Timer

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(Activeball)
	vpmTimer.PulseSw(26)
	RandomSoundSlingshotLeft zCol_Rubber_Post_LS2
	LStep = -1 : LeftSlingShot_Timer ' Initialize Step to 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
End Sub
 
 Sub LeftSlingShot_Timer
	Dim x1, x2, y: x1 = True:x2 = False:y = -20
    Select Case LStep
        Case 3:x1 = False:x2 = True:y = -10
        Case 4:x1 = False:x2 = False:y = 0:LeftSlingShot.TimerEnabled = 0
    End Select
	LSling1_BM_Dark_Room.Visible = x1 ' VLM.Props;BM;1;LSling1
	LSling1_LM_Lit_Room.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_GI_Red_l101a.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Gi_White_l100b.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Flashers_f18.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Flashers_f21.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling2_BM_Dark_Room.Visible = x2 ' VLM.Props;BM;1;LSling2
	LSling2_LM_Lit_Room.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_Gi_Blue_l102a.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_GI_Red_l101a.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_Gi_White_l100b.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_Flashers_f18.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_Flashers_f21.Visible = x2 ' VLM.Props;LM;1;LSling2
	LEMK_BM_Dark_Room.transy = y ' VLM.Props;BM;1;LEMK
	LEMK_LM_Lit_Room.transy = y ' VLM.Props;LM;1;LEMK
	LEMK_LM_Gi_Blue_l102a.transy = y ' VLM.Props;LM;1;LEMK
	LEMK_LM_GI_Red_l101a.transy = y ' VLM.Props;LM;1;LEMK
	LEMK_LM_Gi_White_l100a.transy = y ' VLM.Props;LM;1;LEMK
	LEMK_LM_Gi_White_l100b.transy = y ' VLM.Props;LM;1;LEMK
	LEMK_LM_Inserts_L28.transy = y ' VLM.Props;LM;1;LEMK
	LEMK_LM_Flashers_f21.transy = y ' VLM.Props;LM;1;LEMK
    LStep = LStep + 1
 End Sub
 
Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(Activeball)
	vpmTimer.PulseSw(27)
	RandomSoundSlingshotRight zCol_Rubber_Post_RS2
	RStep = -1 : RightSlingShot_Timer ' Initialize Step to 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
End Sub
 
Sub RightSlingShot_Timer
	Dim x1, x2, y: x1 = True:x2 = False:y = -20
	Select Case RStep
		Case 3:x1 = False:x2 = True:y = -10
		Case 4:x1 = False:x2 = False:y = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RSling1_BM_Dark_Room.Visible = x1 ' VLM.Props;BM;1;RSling1
	RSling1_LM_Lit_Room.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Gi_l103b.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Gi_Blue_l102b.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_GI_Red_l101b.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Gi_White_l100d.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Flashers_f21.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling2_BM_Dark_Room.Visible = x2 ' VLM.Props;BM;1;RSling2
	RSling2_LM_Lit_Room.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Gi_l103b.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_GI_Red_l101b.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Gi_White_l100d.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Flashers_f17.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Flashers_f18.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Flashers_f21.Visible = x2 ' VLM.Props;LM;1;RSling2
	REMK_BM_Dark_Room.transy = y ' VLM.Props;BM;1;REMK
	REMK_LM_Lit_Room.transy = y ' VLM.Props;LM;1;REMK
	REMK_LM_Gi_l103b.transy = y ' VLM.Props;LM;1;REMK
	REMK_LM_Gi_Blue_l102b.transy = y ' VLM.Props;LM;1;REMK
	REMK_LM_GI_Red_l101b.transy = y ' VLM.Props;LM;1;REMK
	REMK_LM_Gi_White_l100c.transy = y ' VLM.Props;LM;1;REMK
	REMK_LM_Gi_White_l100d.transy = y ' VLM.Props;LM;1;REMK
	REMK_LM_Inserts_L17.transy = y ' VLM.Props;LM;1;REMK
	REMK_LM_Flashers_f21.transy = y ' VLM.Props;LM;1;REMK
	RStep = RStep + 1
End Sub

Const IMPowerSetting = 45
Const IMTime = 0.6
Set plungerIM = New cvpmImpulseP
With plungerIM
	.InitImpulseP swplunger, IMPowerSetting, IMTime
	.Random 0
	.CreateEvents "plungerIM"
End With

'Bumpers
Sub Bumper1b_Hit:vpmTimer.PulseSw 31:RandomSoundBumperTop Bumper1b:End Sub
Sub Bumper2b_Hit:vpmTimer.PulseSw 30:RandomSoundBumperMiddle Bumper2b:End Sub
Sub Bumper3b_Hit:vpmTimer.PulseSw 32:RandomSoundBumperBottom Bumper3b:End Sub

'*******************************************
'  Ramp Triggers
'*******************************************


Sub TriggerShooterStart_hit()
    If activeball.vely < 0 Then
        WireRampOn False	'On Wire Ramp (Shooter Lane) Play Wire Ramp Sound
    Else
        WireRampOff
    End If
End Sub

Sub TriggerShooterExit_hit()
	WireRampOff				'Exiting Wire Ramp (Shooter Lane) Stop Playing Sound
End Sub

Sub TriggerLeftRamp1_hit()
    If activeball.vely < 0 Then
        WireRampOn True		'Play Plastic Ramp Sound
    Else
        WireRampOff
    End If
End Sub

Sub TriggerLeftRamp2_hit()
        WireRampOn True		'Play Plastic Ramp Sound	
End Sub

Sub TriggerRightRamp1_hit()
    If activeball.vely < 0 Then
        WireRampOn True		'Play Plastic Ramp Sound
    Else
        WireRampOff
    End If
End Sub

'Sub IceRampEndR_hit()
'	WireRampOff				'Exiting Wire Ramp (Lower Iceman Exit) Stop Playing Sound
'	IceManHold = IceManHold - 1
'End Sub
'
'Sub IceRampEndL_hit()
'	WireRampOff
'	IceManHold = IceManHold - 1
'	WireRampOn True			'Exiting Wire Ramp (Left Iceman Exit) Play Plastic Ramp Sound
'End Sub

Sub TriggerIceRampEnd_unhit()
	WireRampOff
	BallOnIceRamp(activeball.id) = False
End Sub

Sub TriggerLeftRampEnd_hit()
	WireRampOff				'Exiting Wire Ramp (Lower Iceman Exit) Stop Playing Sound
End Sub

'**********************************

Sub swPlunger_Hit:BallinPlunger = 1:End Sub
Sub swPlunger_UnHit:BallinPlunger = 0:End Sub
' 
'' Create ball in kicker and assign a Ball ID used mostly in non-vpm tables
'Sub CreateBallID(Kickername)
'    For cnt = 1 to ubound(ballStatus)
'        If ballStatus(cnt) = 0 Then
'            Set currentball(cnt) = Kickername.createball
'            currentball(cnt).uservalue = cnt
'            ballStatus(cnt) = 1
'            ballStatus(0) = ballStatus(0) + 1
'            If coff = False Then
'                If ballStatus(0)> 1 and XYdata.enabled = False Then XYdata.enabled = True
'            End If
'            Exit For
'        End If
'    Next
'End Sub
' 
'Sub ClearBallID
'    On Error Resume Next
'    iball = ActiveBall.uservalue
'    currentball(iball).UserValue = 0
'    If Err Then Msgbox Err.description & vbCrLf & iball
'    ballStatus(iBall) = 0
'    ballStatus(0) = ballStatus(0) -1
'    On Error Goto 0
'End Sub
 
'*******************************
'****Flipper Prims / Shadows****
'*******************************
 
 Dim RFLogo1Fade : RFLogo1Fade = 100.0
 
Sub UpdateFlipperLogo
	Dim lfa : lfa = LeftFlipper.CurrentAngle
    FlipperLSh.RotZ = lfa
	LFLogo_BM_Dark_Room.RotZ = lfa ' VLM.Props;BM;1;LFLogo
	LFLogo_LM_Lit_Room.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Gi_Blue_l102a.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_GI_Red_l101a.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Gi_White_l100a.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Gi_White_l100b.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Gi_White_l100c.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Gi_White_l100d.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Inserts_L17.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Inserts_L18.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Inserts_L19.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Inserts_L28.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Inserts_L29.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Inserts_L30.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Inserts_L31.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Inserts_L32.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Flashers_f17.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	LFLogo_LM_Flashers_f21.RotZ = lfa ' VLM.Props;LM;1;LFLogo
	Dim rfa : rfa = RightFlipper.CurrentAngle
	FlipperRSh.RotZ = rfa
	RFLogo_BM_Dark_Room.RotZ = rfa ' VLM.Props;BM;1;RFLogo
	RFLogo_LM_Lit_Room.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Gi_Blue_l102b.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_GI_Red_l101b.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Gi_White_l100a.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Gi_White_l100b.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Gi_White_l100c.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Gi_White_l100d.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Inserts_L17.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Inserts_L18.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Inserts_L19.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Inserts_L28.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Inserts_L29.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Inserts_L30.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Inserts_L31.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Inserts_L32.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	RFLogo_LM_Flashers_f21.RotZ = rfa ' VLM.Props;LM;1;RFLogo
	Dim rfa1 : rfa1 = RightFlipper1.CurrentAngle
	FlipperRSh1.RotZ = rfa1
	RFLogo1_BM_Dark_Room.RotZ = rfa1 ' VLM.Props;BM;1;RFLogo1
	RFLogo1_LM_Lit_Room.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Gi_l103b.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Gi_Blue_l102.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_GI_Red_l101d.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Gi_White_l100.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Gi_White_l100h.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Inserts_L66.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Inserts_L71.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Inserts_L72.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Flashers_f18.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Flashers_f19.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Flashers_f20.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Flashers_f21.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1_LM_Flashers_f25.RotZ = rfa1 ' VLM.Props;LM;1;RFLogo1
	RFLogo1Fade = 25.0 + 75.0 * (260.0 - rfa1) / 58.0
	RFLogo1_LM_Gi_White_l100.opacity = RFLogo1Fade * lampz.lvl(100) / 255
	RFLogo1_LM_Gi_White_l100h.opacity = RFLogo1Fade * lampz.lvl(100) / 255
	RFLogo1_LM_GI_Red_l101d.opacity   = RFLogo1Fade * lampz.lvl(101) / 255
	RFLogo1_LM_Gi_Blue_l102.opacity  = RFLogo1Fade * lampz.lvl(102) / 255
End Sub
 
'************************************************************************
' ZGI                             Color GI
'************************************************************************
 
SolModCallback(54) = "SolGIWhite" ' GI White (Sol 44 in manual)
SolModCallback(55) = "SolGIRed" ' GI Red (Sol 45 in manual)
SolModCallback(56) = "SolGIBlue" ' GI Blue (Sol 46 in manual)
set GICallback = GetRef("ChangeGI")
'set GICallback2 = GetRef("ChangeGI2") ' Main GI is not modulated (either 0 or 9)

Dim GI_State, GI_WhiteOff, GI_RedOff, GI_BlueOff
GI_State = 0: GI_WhiteOff = 0: GI_RedOff = 0: GI_BlueOff = 0
Sub SolGIWhite(value) : GI_WhiteOff = value : UpdateGI : End Sub
Sub SolGIRed(value) : GI_RedOff = value : UpdateGI : End Sub
Sub SolGIBlue(value) : GI_BlueOff = value : UpdateGI : End Sub
Sub ChangeGI(nr, enabled) : GI_State = enabled : UpdateGI : End Sub
 
Sub UpdateGI
    dim bulb, GIColor
	If GI_State Then
		PinCab_Backglass.blenddisablelighting = 5
		Lampz.state(100) = 255 - GI_WhiteOff
		Lampz.state(101) = 255 - GI_RedOff
		Lampz.state(102) = 255 - GI_BlueOff
		Lampz.state(103) = 255
	Else
		PinCab_Backglass.blenddisablelighting = 0.2
		Lampz.state(100) = 0
		Lampz.state(101) = 0
		Lampz.state(102) = 0
		Lampz.state(103) = 0
	End If

	' DOF colors and ball reflection colors (unused at the moment)
	GIColor = RGB(255 - (GI_WhiteOff + GI_RedOff) / 2, 255 - GI_WhiteOff, 255 - (GI_WhiteOff + GI_BlueOff) / 2)
	if GI_RedOff = 0 AND GI_BlueOff > 0 AND GI_WhiteOff > 0 then GIColor = RGB(200,0,0):DOF 200, DOFOn:DOF 201, DOFOff:DOF 202, DOFOff
	if GI_RedOff > 0 AND GI_BlueOff = 0 AND GI_WhiteOff > 0 then GIColor = RGB(0,0,200):DOF 200, DOFOff:DOF 201, DOFOn:DOF 202, DOFOff
	if GI_RedOff = 0 AND GI_BlueOff = 0 AND GI_WhiteOff = 0 then GIColor = RGB(255,255,255):DOF 200, DOFOff:DOF 201, DOFOff:DOF 202, DOFOn
    For Each bulb In GI_Color
        If GIColor > 0 And GI_State Then
            bulb.state = 1
        Else
            bulb.state = 0
        End If
        bulb.color = RGB(0,0,0)
        bulb.colorfull = GIColor
    Next
End Sub
 
Sub RLS()
'    SpinnerT1.RotZ = -(sw9.currentangle)
'   ampprim.objrotx = ampf.currentangle
	Dim g1, g2, g3, g4, g5
	g1 = -(sw11.currentangle) ' RampGate1
	RampGate1_BM_Dark_Room.RotX = g1 ' VLM.Props;BM;1;RampGate1
	RampGate1_LM_Lit_Room.RotX = g1 ' VLM.Props;LM;1;RampGate1
	RampGate1_LM_Inserts_L43.RotX = g1 ' VLM.Props;LM;1;RampGate1
	RampGate1_LM_Flashers_f17.RotX = g1 ' VLM.Props;LM;1;RampGate1
	RampGate1_LM_Flashers_f25.RotX = g1 ' VLM.Props;LM;1;RampGate1

    g2 = -(sw13.currentangle) ' RampGate2
	RampGate2_BM_Dark_Room.RotX = g2 ' VLM.Props;BM;1;RampGate2
	RampGate2_LM_Lit_Room.RotX = g2 ' VLM.Props;LM;1;RampGate2

    g3 = -(gate3.currentangle) ' RampGate3
	RampGate3_BM_Dark_Room.RotX = g3 ' VLM.Props;BM;1;RampGate3
	RampGate3_LM_Lit_Room.RotX = g3 ' VLM.Props;LM;1;RampGate3
	RampGate3_LM_Flashers_f18.RotX = g3 ' VLM.Props;LM;1;RampGate3

    g4 = -(sw48.currentangle) ' RampGate4
	RampGate4_BM_Dark_Room.RotX = g4 ' VLM.Props;BM;1;RampGate4
	RampGate4_LM_Lit_Room.RotX = g4 ' VLM.Props;LM;1;RampGate4
	RampGate4_LM_Gi_White_l100k.RotX = g4 ' VLM.Props;LM;1;RampGate4
	RampGate4_LM_Flashers_f18.RotX = g4 ' VLM.Props;LM;1;RampGate4

    g5 = -(sw47.currentangle)
	SpinnerT2_BM_Dark_Room.RotY = g5 ' VLM.Props;BM;1;SpinnerT2
	SpinnerT2_LM_Flashers_f28.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Lit_Room.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_GI_Red_l101f.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Gi_White_l100.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Gi_White_l100k.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Inserts_L42.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Inserts_L66.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Inserts_L73.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Inserts_L75.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Inserts_L76.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Inserts_L77.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Flashers_f17.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Flashers_f18.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Flashers_f19.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Flashers_f20.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Flashers_f22.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
	SpinnerT2_LM_Flashers_f30.RotY = g5 ' VLM.Props;LM;1;SpinnerT2
End Sub
 
Sub uppostt()
    If lockpin1.isdropped = true then uppostf.rotatetoend
    if lockpin1.isdropped = false then uppostf.rotatetostart
    Dim z : z = uppostf.currentangle - 55
	uppostp_BM_Dark_Room.transz = z ' VLM.Props;BM;1;uppostp
	uppostp_LM_Lit_Room.transz = z ' VLM.Props;LM;1;uppostp
	uppostp_LM_Inserts_L67.transz = z ' VLM.Props;LM;1;uppostp
	uppostp_LM_Flashers_f18.transz = z ' VLM.Props;LM;1;uppostp
	uppostp_LM_Flashers_f22.transz = z ' VLM.Props;LM;1;uppostp
	uppostp_LM_Flashers_f30.transz = z ' VLM.Props;LM;1;uppostp
End Sub


'******************************************
'    Wolverine Wobble
'******************************************

Const WobbleScale = 1

WolverineWobble.interval = 34 ' Controls the speed of the wobble
Dim WobbleValue:WobbleValue = 0
Sub WolverineWobble_timer
    VengeanceMove WobbleValue

    if WobbleValue < 0 then
        WobbleValue = abs(WobbleValue) * 0.9 - 0.1
    Else
        WobbleValue = -abs(WobbleValue) * 0.9 + 0.1
    end if

    if abs(WobbleValue) < 0.1 Then
        WobbleValue = 0
		VengeanceMove WobbleValue
        WolverineWobble.Enabled = False
    end If
End Sub


Sub VengeanceMove(Vmove)
    Dim a : a = 180 + WobbleValue * WobbleScale
	Wolverine_BM_Dark_Room.RotZ = a ' VLM.Props;BM;1;Wolverine
	Wolverine_LM_Lit_Room.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Gi_l103a.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Gi_l103b.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Gi_Blue_l102d.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Gi_Blue_l102.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Gi_Blue_l102f.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_GI_Red_l101c.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Gi_White_l100.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L37.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L38.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L41.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L42.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L45.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L46.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L52.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L54.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L59.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Inserts_L65.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Flashers_f17.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Flashers_f18.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Flashers_f19.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Flashers_f20.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Flashers_f21.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Flashers_f22.RotZ = a ' VLM.Props;LM;1;Wolverine
	Wolverine_LM_Flashers_f25.RotZ = a ' VLM.Props;LM;1;Wolverine
End Sub
 

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
	'chglamp = Controller.ChangedSolenoids
	'If Not IsEmpty(chglamp) Then
		'For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			'Debug.print "> Chg " & chglamp(x, 0) & " => " & chglamp(x, 1)
		'next
	'End If
	Lampz.Update2	'update (fading logic only)
End Sub

'dim FrameTime, InitFrameTime : InitFrameTime = 0
'LampTimer2.Interval = -1
'LampTimer2.Enabled = True
'Sub LampTimer2_Timer()
'	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
'	Lampz.Update 'updates on frametime (Object updates only)
'End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub UpdateLightMap(lightmap, intensity, ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	lightmap.Opacity = aLvl * intensity
End Sub

Sub InitLampsNF()
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	' The following change are applied to the default lighting:
	' - Lights 125, 41, 43 and 59 must use IcemanFade as intensity for the Iceman lightmap
	' - Lights 117 must use 50 + 0.5 * IcemanFade as intensity for the Iceman lightmap
	' - Flasher 131 intensity must be raised to 300 for correct lighting of apron plastics

	'Adjust fading speeds (max level / full MS fading time)
	Dim x
	for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/120 : next
	' GI
	for x = 100 to 103 : Lampz.FadeSpeedUp(x) = 255/40 : Lampz.FadeSpeedDown(x) = 255/120 : Lampz.Modulate(x) = 1/255 : next
	'for x = 100 to 103 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/8 : Lampz.Modulate(x) = 1 : next
	For each x in GI_White : Lampz.MassAssign(100) = x: Next
	For each x in GI_Red : Lampz.MassAssign(101) = x: Next
	For each x in GI_Blue : Lampz.MassAssign(102) = x: Next
	For each x in GI_Base : Lampz.MassAssign(103) = x: Next
	' FLashers
	for x = 117 to 132 : Lampz.FadeSpeedUp(x) = 255/40 : Lampz.FadeSpeedDown(x) = 255/400 : Lampz.Modulate(x) = 1.0/255 : next
	' Hide all lights used for ball reflection only (only hide the direct halo and the transmission part, not the reflection on balls)
	For each x in BallReflections : x.visible = False : Next
	' Room lighting
	Lampz.FadeSpeedUp(150) = 100/1 : Lampz.FadeSpeedDown(150) = 100/1 : Lampz.Modulate(150) = 1/100

	' Lampz.MassAssign(150) = L150 ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Playfield_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Layer_1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Layer_2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Layer_3_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Layer_4_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Layer_5_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap BR1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap BR2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap BR3_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Disc_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Div_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap LEMK_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap LFLogo_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap LSling1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap LSling2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap OutPostL_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap OutPostR_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap REMK_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RFLogo_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RFLogo1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RSling1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RSling2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RampGate1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RampGate2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RampGate3_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RampGate4_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap SpinnerT2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Target_001_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Target_004_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw1p_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw24_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw25_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw28_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw29_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw2p_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw33_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw49_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw53_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw54_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw7p_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw8p_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap uppostp_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Iceman_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Wolverine_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap ApronCol_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap ApronMag_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap BBird_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Sentinel_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap NCL_Bot_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap NCL_Top_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap NCR_Bot_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap NCR_Top_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Playfield_LM_Lit_Room_001, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Playfield_LM_Lit_Room_002, 100.0, " ' VLM.Lampz;Lit Room
	

	Lampz.MassAssign(100) = l100a ' VLM.Lampz;Gi White-l100a
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100a, 100.0, " ' VLM.Lampz;Gi White-l100a
	Lampz.Callback(100) = "UpdateLightMap LEMK_LM_Gi_White_l100a, 100.0, " ' VLM.Lampz;Gi White-l100a
	Lampz.Callback(100) = "UpdateLightMap LFLogo_LM_Gi_White_l100a, 100.0, " ' VLM.Lampz;Gi White-l100a
	Lampz.Callback(100) = "UpdateLightMap RFLogo_LM_Gi_White_l100a, 100.0, " ' VLM.Lampz;Gi White-l100a
	Lampz.Callback(100) = "UpdateLightMap ApronMag_LM_Gi_White_l100a, 100.0, " ' VLM.Lampz;Gi White-l100a
	Lampz.MassAssign(100) = l100i ' VLM.Lampz;Gi White-l100i
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100i, 100.0, " ' VLM.Lampz;Gi White-l100i
	Lampz.Callback(100) = "UpdateLightMap Layer_1_LM_Gi_White_l100i, 100.0, " ' VLM.Lampz;Gi White-l100i
	Lampz.Callback(100) = "UpdateLightMap Layer_4_LM_Gi_White_l100i, 100.0, " ' VLM.Lampz;Gi White-l100i
	Lampz.Callback(100) = "UpdateLightMap BR2_LM_Gi_White_l100i, 100.0, " ' VLM.Lampz;Gi White-l100i
	' Lampz.MassAssign(100) = l100 ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap Layer_1_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap Layer_3_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap Layer_4_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap BR1_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap RFLogo1_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap SpinnerT2_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap sw7p_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap Iceman_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap Wolverine_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.Callback(100) = "UpdateLightMap NCR_Top_LM_Gi_White_l100, 100.0, " ' VLM.Lampz;Gi White-l100
	Lampz.MassAssign(100) = l100j ' VLM.Lampz;Gi White-l100j
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100j, 100.0, " ' VLM.Lampz;Gi White-l100j
	Lampz.Callback(100) = "UpdateLightMap Layer_1_LM_Gi_White_l100j, 100.0, " ' VLM.Lampz;Gi White-l100j
	Lampz.Callback(100) = "UpdateLightMap Layer_4_LM_Gi_White_l100j, 100.0, " ' VLM.Lampz;Gi White-l100j
	Lampz.Callback(100) = "UpdateLightMap sw54_LM_Gi_White_l100j, 100.0, " ' VLM.Lampz;Gi White-l100j
	Lampz.MassAssign(100) = l100k ' VLM.Lampz;Gi White-l100k
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100k, 100.0, " ' VLM.Lampz;Gi White-l100k
	Lampz.Callback(100) = "UpdateLightMap Layer_1_LM_Gi_White_l100k, 100.0, " ' VLM.Lampz;Gi White-l100k
	Lampz.Callback(100) = "UpdateLightMap Layer_2_LM_Gi_White_l100k, 100.0, " ' VLM.Lampz;Gi White-l100k
	Lampz.Callback(100) = "UpdateLightMap Layer_3_LM_Gi_White_l100k, 100.0, " ' VLM.Lampz;Gi White-l100k
	Lampz.Callback(100) = "UpdateLightMap Layer_5_LM_Gi_White_l100k, 100.0, " ' VLM.Lampz;Gi White-l100k
	Lampz.Callback(100) = "UpdateLightMap RampGate4_LM_Gi_White_l100k, 100.0, " ' VLM.Lampz;Gi White-l100k
	Lampz.Callback(100) = "UpdateLightMap SpinnerT2_LM_Gi_White_l100k, 100.0, " ' VLM.Lampz;Gi White-l100k
	Lampz.Callback(100) = "UpdateLightMap Sentinel_LM_Gi_White_l100k, 100.0, " ' VLM.Lampz;Gi White-l100k
	Lampz.MassAssign(100) = l100b ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap Layer_4_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap LEMK_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap LFLogo_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap LSling1_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap LSling2_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap RFLogo_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap sw24_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.Callback(100) = "UpdateLightMap sw25_LM_Gi_White_l100b, 100.0, " ' VLM.Lampz;Gi White-l100b
	Lampz.MassAssign(100) = l100c ' VLM.Lampz;Gi White-l100c
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100c, 100.0, " ' VLM.Lampz;Gi White-l100c
	Lampz.Callback(100) = "UpdateLightMap LFLogo_LM_Gi_White_l100c, 100.0, " ' VLM.Lampz;Gi White-l100c
	Lampz.Callback(100) = "UpdateLightMap REMK_LM_Gi_White_l100c, 100.0, " ' VLM.Lampz;Gi White-l100c
	Lampz.Callback(100) = "UpdateLightMap RFLogo_LM_Gi_White_l100c, 100.0, " ' VLM.Lampz;Gi White-l100c
	Lampz.Callback(100) = "UpdateLightMap ApronMag_LM_Gi_White_l100c, 100.0, " ' VLM.Lampz;Gi White-l100c
	Lampz.MassAssign(100) = l100d ' VLM.Lampz;Gi White-l100d
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100d, 100.0, " ' VLM.Lampz;Gi White-l100d
	Lampz.Callback(100) = "UpdateLightMap LFLogo_LM_Gi_White_l100d, 100.0, " ' VLM.Lampz;Gi White-l100d
	Lampz.Callback(100) = "UpdateLightMap REMK_LM_Gi_White_l100d, 100.0, " ' VLM.Lampz;Gi White-l100d
	Lampz.Callback(100) = "UpdateLightMap RFLogo_LM_Gi_White_l100d, 100.0, " ' VLM.Lampz;Gi White-l100d
	Lampz.Callback(100) = "UpdateLightMap RSling1_LM_Gi_White_l100d, 100.0, " ' VLM.Lampz;Gi White-l100d
	Lampz.Callback(100) = "UpdateLightMap RSling2_LM_Gi_White_l100d, 100.0, " ' VLM.Lampz;Gi White-l100d
	Lampz.Callback(100) = "UpdateLightMap sw28_LM_Gi_White_l100d, 100.0, " ' VLM.Lampz;Gi White-l100d
	Lampz.MassAssign(100) = l100e ' VLM.Lampz;Gi White-l100e
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100e, 100.0, " ' VLM.Lampz;Gi White-l100e
	Lampz.Callback(100) = "UpdateLightMap Layer_1_LM_Gi_White_l100e, 100.0, " ' VLM.Lampz;Gi White-l100e
	Lampz.Callback(100) = "UpdateLightMap Layer_4_LM_Gi_White_l100e, 100.0, " ' VLM.Lampz;Gi White-l100e
	Lampz.Callback(100) = "UpdateLightMap OutPostL_LM_Gi_White_l100e, 100.0, " ' VLM.Lampz;Gi White-l100e
	Lampz.Callback(100) = "UpdateLightMap sw1p_LM_Gi_White_l100e, 100.0, " ' VLM.Lampz;Gi White-l100e
	Lampz.MassAssign(100) = l100f ' VLM.Lampz;Gi White-l100f
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100f, 100.0, " ' VLM.Lampz;Gi White-l100f
	Lampz.Callback(100) = "UpdateLightMap Layer_1_LM_Gi_White_l100f, 100.0, " ' VLM.Lampz;Gi White-l100f
	Lampz.Callback(100) = "UpdateLightMap Layer_4_LM_Gi_White_l100f, 100.0, " ' VLM.Lampz;Gi White-l100f
	Lampz.Callback(100) = "UpdateLightMap BR3_LM_Gi_White_l100f, 100.0, " ' VLM.Lampz;Gi White-l100f
	Lampz.Callback(100) = "UpdateLightMap sw1p_LM_Gi_White_l100f, 100.0, " ' VLM.Lampz;Gi White-l100f
	Lampz.Callback(100) = "UpdateLightMap sw2p_LM_Gi_White_l100f, 100.0, " ' VLM.Lampz;Gi White-l100f
	Lampz.MassAssign(100) = l100g ' VLM.Lampz;Gi White-l100g
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100g, 100.0, " ' VLM.Lampz;Gi White-l100g
	Lampz.Callback(100) = "UpdateLightMap Layer_1_LM_Gi_White_l100g, 100.0, " ' VLM.Lampz;Gi White-l100g
	Lampz.Callback(100) = "UpdateLightMap Layer_3_LM_Gi_White_l100g, 100.0, " ' VLM.Lampz;Gi White-l100g
	Lampz.Callback(100) = "UpdateLightMap OutPostR_LM_Gi_White_l100g, 100.0, " ' VLM.Lampz;Gi White-l100g
	Lampz.Callback(100) = "UpdateLightMap sw8p_LM_Gi_White_l100g, 100.0, " ' VLM.Lampz;Gi White-l100g
	Lampz.Callback(100) = "UpdateLightMap Iceman_LM_Gi_White_l100g, 100.0, " ' VLM.Lampz;Gi White-l100g
	Lampz.MassAssign(100) = l100h ' VLM.Lampz;Gi White-l100h
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_Gi_White_l100h, 100.0, " ' VLM.Lampz;Gi White-l100h
	Lampz.Callback(100) = "UpdateLightMap Layer_1_LM_Gi_White_l100h, 100.0, " ' VLM.Lampz;Gi White-l100h
	Lampz.Callback(100) = "UpdateLightMap Layer_3_LM_Gi_White_l100h, 100.0, " ' VLM.Lampz;Gi White-l100h
	Lampz.Callback(100) = "UpdateLightMap RFLogo1_LM_Gi_White_l100h, 100.0, " ' VLM.Lampz;Gi White-l100h
	Lampz.Callback(100) = "UpdateLightMap sw7p_LM_Gi_White_l100h, 100.0, " ' VLM.Lampz;Gi White-l100h
	Lampz.Callback(100) = "UpdateLightMap sw8p_LM_Gi_White_l100h, 100.0, " ' VLM.Lampz;Gi White-l100h

	Lampz.MassAssign(101) = l101a ' VLM.Lampz;GI Red-l101a
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_Red_l101a, 100.0, " ' VLM.Lampz;GI Red-l101a
	Lampz.Callback(101) = "UpdateLightMap Layer_4_LM_GI_Red_l101a, 100.0, " ' VLM.Lampz;GI Red-l101a
	Lampz.Callback(101) = "UpdateLightMap LEMK_LM_GI_Red_l101a, 100.0, " ' VLM.Lampz;GI Red-l101a
	Lampz.Callback(101) = "UpdateLightMap LFLogo_LM_GI_Red_l101a, 100.0, " ' VLM.Lampz;GI Red-l101a
	Lampz.Callback(101) = "UpdateLightMap LSling1_LM_GI_Red_l101a, 100.0, " ' VLM.Lampz;GI Red-l101a
	Lampz.Callback(101) = "UpdateLightMap LSling2_LM_GI_Red_l101a, 100.0, " ' VLM.Lampz;GI Red-l101a
	Lampz.Callback(101) = "UpdateLightMap sw24_LM_GI_Red_l101a, 100.0, " ' VLM.Lampz;GI Red-l101a
	Lampz.Callback(101) = "UpdateLightMap sw25_LM_GI_Red_l101a, 100.0, " ' VLM.Lampz;GI Red-l101a
	Lampz.MassAssign(101) = l101g ' VLM.Lampz;GI Red-l101g
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_Red_l101g, 100.0, " ' VLM.Lampz;GI Red-l101g
	Lampz.Callback(101) = "UpdateLightMap Layer_3_LM_GI_Red_l101g, 100.0, " ' VLM.Lampz;GI Red-l101g
	Lampz.Callback(101) = "UpdateLightMap Sentinel_LM_GI_Red_l101g, 100.0, " ' VLM.Lampz;GI Red-l101g
	Lampz.MassAssign(101) = l101b ' VLM.Lampz;GI Red-l101b
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_Red_l101b, 100.0, " ' VLM.Lampz;GI Red-l101b
	Lampz.Callback(101) = "UpdateLightMap REMK_LM_GI_Red_l101b, 100.0, " ' VLM.Lampz;GI Red-l101b
	Lampz.Callback(101) = "UpdateLightMap RFLogo_LM_GI_Red_l101b, 100.0, " ' VLM.Lampz;GI Red-l101b
	Lampz.Callback(101) = "UpdateLightMap RSling1_LM_GI_Red_l101b, 100.0, " ' VLM.Lampz;GI Red-l101b
	Lampz.Callback(101) = "UpdateLightMap RSling2_LM_GI_Red_l101b, 100.0, " ' VLM.Lampz;GI Red-l101b
	Lampz.Callback(101) = "UpdateLightMap sw28_LM_GI_Red_l101b, 100.0, " ' VLM.Lampz;GI Red-l101b
	Lampz.Callback(101) = "UpdateLightMap sw33_LM_GI_Red_l101b, 100.0, " ' VLM.Lampz;GI Red-l101b
	Lampz.MassAssign(101) = l101c ' VLM.Lampz;GI Red-l101c
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_Red_l101c, 100.0, " ' VLM.Lampz;GI Red-l101c
	Lampz.Callback(101) = "UpdateLightMap Layer_1_LM_GI_Red_l101c, 100.0, " ' VLM.Lampz;GI Red-l101c
	Lampz.Callback(101) = "UpdateLightMap Layer_4_LM_GI_Red_l101c, 100.0, " ' VLM.Lampz;GI Red-l101c
	Lampz.Callback(101) = "UpdateLightMap BR3_LM_GI_Red_l101c, 100.0, " ' VLM.Lampz;GI Red-l101c
	Lampz.Callback(101) = "UpdateLightMap sw1p_LM_GI_Red_l101c, 100.0, " ' VLM.Lampz;GI Red-l101c
	Lampz.Callback(101) = "UpdateLightMap sw2p_LM_GI_Red_l101c, 100.0, " ' VLM.Lampz;GI Red-l101c
	Lampz.Callback(101) = "UpdateLightMap Wolverine_LM_GI_Red_l101c, 100.0, " ' VLM.Lampz;GI Red-l101c
	Lampz.MassAssign(101) = l101d ' VLM.Lampz;GI Red-l101d
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_Red_l101d, 100.0, " ' VLM.Lampz;GI Red-l101d
	Lampz.Callback(101) = "UpdateLightMap Layer_1_LM_GI_Red_l101d, 100.0, " ' VLM.Lampz;GI Red-l101d
	Lampz.Callback(101) = "UpdateLightMap Layer_3_LM_GI_Red_l101d, 100.0, " ' VLM.Lampz;GI Red-l101d
	Lampz.Callback(101) = "UpdateLightMap RFLogo1_LM_GI_Red_l101d, 100.0, " ' VLM.Lampz;GI Red-l101d
	Lampz.Callback(101) = "UpdateLightMap sw7p_LM_GI_Red_l101d, 100.0, " ' VLM.Lampz;GI Red-l101d
	Lampz.Callback(101) = "UpdateLightMap sw8p_LM_GI_Red_l101d, 100.0, " ' VLM.Lampz;GI Red-l101d
	Lampz.Callback(101) = "UpdateLightMap Iceman_LM_GI_Red_l101d, 100.0, " ' VLM.Lampz;GI Red-l101d
	Lampz.MassAssign(101) = l101e ' VLM.Lampz;GI Red-l101e
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_Red_l101e, 100.0, " ' VLM.Lampz;GI Red-l101e
	Lampz.Callback(101) = "UpdateLightMap Layer_1_LM_GI_Red_l101e, 100.0, " ' VLM.Lampz;GI Red-l101e
	Lampz.Callback(101) = "UpdateLightMap Layer_4_LM_GI_Red_l101e, 100.0, " ' VLM.Lampz;GI Red-l101e
	Lampz.MassAssign(101) = l101h ' VLM.Lampz;GI Red-l101h
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_Red_l101h, 100.0, " ' VLM.Lampz;GI Red-l101h
	Lampz.Callback(101) = "UpdateLightMap Layer_1_LM_GI_Red_l101h, 100.0, " ' VLM.Lampz;GI Red-l101h
	Lampz.Callback(101) = "UpdateLightMap Layer_4_LM_GI_Red_l101h, 100.0, " ' VLM.Lampz;GI Red-l101h
	Lampz.Callback(101) = "UpdateLightMap Target_004_LM_GI_Red_l101h, 100.0, " ' VLM.Lampz;GI Red-l101h
	Lampz.MassAssign(101) = l101f ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Layer_1_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Layer_2_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Layer_3_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Layer_4_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Layer_5_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Div_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap SpinnerT2_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Target_001_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f
	Lampz.Callback(101) = "UpdateLightMap Sentinel_LM_GI_Red_l101f, 100.0, " ' VLM.Lampz;GI Red-l101f

	Lampz.MassAssign(102) = l102a ' VLM.Lampz;Gi Blue-l102a
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_Gi_Blue_l102a, 100.0, " ' VLM.Lampz;Gi Blue-l102a
	Lampz.Callback(102) = "UpdateLightMap Layer_4_LM_Gi_Blue_l102a, 100.0, " ' VLM.Lampz;Gi Blue-l102a
	Lampz.Callback(102) = "UpdateLightMap LEMK_LM_Gi_Blue_l102a, 100.0, " ' VLM.Lampz;Gi Blue-l102a
	Lampz.Callback(102) = "UpdateLightMap LFLogo_LM_Gi_Blue_l102a, 100.0, " ' VLM.Lampz;Gi Blue-l102a
	Lampz.Callback(102) = "UpdateLightMap LSling2_LM_Gi_Blue_l102a, 100.0, " ' VLM.Lampz;Gi Blue-l102a
	Lampz.Callback(102) = "UpdateLightMap sw24_LM_Gi_Blue_l102a, 100.0, " ' VLM.Lampz;Gi Blue-l102a
	Lampz.Callback(102) = "UpdateLightMap sw25_LM_Gi_Blue_l102a, 100.0, " ' VLM.Lampz;Gi Blue-l102a
	Lampz.Callback(102) = "UpdateLightMap ApronMag_LM_Gi_Blue_l102a, 100.0, " ' VLM.Lampz;Gi Blue-l102a
	Lampz.MassAssign(102) = l102h ' VLM.Lampz;Gi Blue-l102h
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_Gi_Blue_l102h, 100.0, " ' VLM.Lampz;Gi Blue-l102h
	Lampz.Callback(102) = "UpdateLightMap Layer_3_LM_Gi_Blue_l102h, 100.0, " ' VLM.Lampz;Gi Blue-l102h
	Lampz.Callback(102) = "UpdateLightMap Layer_4_LM_Gi_Blue_l102h, 100.0, " ' VLM.Lampz;Gi Blue-l102h
	Lampz.Callback(102) = "UpdateLightMap sw49_LM_Gi_Blue_l102h, 100.0, " ' VLM.Lampz;Gi Blue-l102h
	Lampz.Callback(102) = "UpdateLightMap Sentinel_LM_Gi_Blue_l102h, 100.0, " ' VLM.Lampz;Gi Blue-l102h
	Lampz.MassAssign(102) = l102b ' VLM.Lampz;Gi Blue-l102b
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_Gi_Blue_l102b, 100.0, " ' VLM.Lampz;Gi Blue-l102b
	Lampz.Callback(102) = "UpdateLightMap REMK_LM_Gi_Blue_l102b, 100.0, " ' VLM.Lampz;Gi Blue-l102b
	Lampz.Callback(102) = "UpdateLightMap RFLogo_LM_Gi_Blue_l102b, 100.0, " ' VLM.Lampz;Gi Blue-l102b
	Lampz.Callback(102) = "UpdateLightMap RSling1_LM_Gi_Blue_l102b, 100.0, " ' VLM.Lampz;Gi Blue-l102b
	Lampz.Callback(102) = "UpdateLightMap sw28_LM_Gi_Blue_l102b, 100.0, " ' VLM.Lampz;Gi Blue-l102b
	Lampz.Callback(102) = "UpdateLightMap sw29_LM_Gi_Blue_l102b, 100.0, " ' VLM.Lampz;Gi Blue-l102b
	Lampz.Callback(102) = "UpdateLightMap ApronMag_LM_Gi_Blue_l102b, 100.0, " ' VLM.Lampz;Gi Blue-l102b
	Lampz.MassAssign(102) = l102c ' VLM.Lampz;Gi Blue-l102c
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_Gi_Blue_l102c, 100.0, " ' VLM.Lampz;Gi Blue-l102c
	Lampz.Callback(102) = "UpdateLightMap Layer_1_LM_Gi_Blue_l102c, 100.0, " ' VLM.Lampz;Gi Blue-l102c
	Lampz.Callback(102) = "UpdateLightMap Layer_4_LM_Gi_Blue_l102c, 100.0, " ' VLM.Lampz;Gi Blue-l102c
	Lampz.Callback(102) = "UpdateLightMap BR3_LM_Gi_Blue_l102c, 100.0, " ' VLM.Lampz;Gi Blue-l102c
	Lampz.Callback(102) = "UpdateLightMap sw1p_LM_Gi_Blue_l102c, 100.0, " ' VLM.Lampz;Gi Blue-l102c
	Lampz.Callback(102) = "UpdateLightMap sw2p_LM_Gi_Blue_l102c, 100.0, " ' VLM.Lampz;Gi Blue-l102c
	' Lampz.MassAssign(102) = l102 ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap Layer_1_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap Layer_3_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap BR1_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap BR3_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap RFLogo1_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap sw7p_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap Iceman_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.Callback(102) = "UpdateLightMap Wolverine_LM_Gi_Blue_l102, 100.0, " ' VLM.Lampz;Gi Blue-l102
	Lampz.MassAssign(102) = l102e ' VLM.Lampz;Gi Blue-l102e
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_Gi_Blue_l102e, 100.0, " ' VLM.Lampz;Gi Blue-l102e
	Lampz.Callback(102) = "UpdateLightMap Layer_1_LM_Gi_Blue_l102e, 100.0, " ' VLM.Lampz;Gi Blue-l102e
	Lampz.Callback(102) = "UpdateLightMap Layer_4_LM_Gi_Blue_l102e, 100.0, " ' VLM.Lampz;Gi Blue-l102e
	Lampz.Callback(102) = "UpdateLightMap BR1_LM_Gi_Blue_l102e, 100.0, " ' VLM.Lampz;Gi Blue-l102e
	Lampz.Callback(102) = "UpdateLightMap BR2_LM_Gi_Blue_l102e, 100.0, " ' VLM.Lampz;Gi Blue-l102e
	Lampz.MassAssign(102) = l102f ' VLM.Lampz;Gi Blue-l102f
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_Gi_Blue_l102f, 100.0, " ' VLM.Lampz;Gi Blue-l102f
	Lampz.Callback(102) = "UpdateLightMap Layer_1_LM_Gi_Blue_l102f, 100.0, " ' VLM.Lampz;Gi Blue-l102f
	Lampz.Callback(102) = "UpdateLightMap Layer_4_LM_Gi_Blue_l102f, 100.0, " ' VLM.Lampz;Gi Blue-l102f
	Lampz.Callback(102) = "UpdateLightMap Wolverine_LM_Gi_Blue_l102f, 100.0, " ' VLM.Lampz;Gi Blue-l102f
	Lampz.MassAssign(102) = l102g ' VLM.Lampz;Gi Blue-l102g
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_Gi_Blue_l102g, 100.0, " ' VLM.Lampz;Gi Blue-l102g
	Lampz.Callback(102) = "UpdateLightMap Layer_3_LM_Gi_Blue_l102g, 100.0, " ' VLM.Lampz;Gi Blue-l102g
	Lampz.Callback(102) = "UpdateLightMap Layer_5_LM_Gi_Blue_l102g, 100.0, " ' VLM.Lampz;Gi Blue-l102g
	Lampz.Callback(102) = "UpdateLightMap Sentinel_LM_Gi_Blue_l102g, 100.0, " ' VLM.Lampz;Gi Blue-l102g
	
	' Lampz.MassAssign(103) = l103 ' VLM.Lampz;Gi-l103
	Lampz.Callback(103) = "UpdateLightMap Playfield_LM_Gi_l103, 100.0, " ' VLM.Lampz;Gi-l103
	Lampz.Callback(103) = "UpdateLightMap Layer_1_LM_Gi_l103, 100.0, " ' VLM.Lampz;Gi-l103
	Lampz.Callback(103) = "UpdateLightMap Layer_3_LM_Gi_l103, 100.0, " ' VLM.Lampz;Gi-l103
	Lampz.Callback(103) = "UpdateLightMap Layer_4_LM_Gi_l103, 100.0, " ' VLM.Lampz;Gi-l103
	Lampz.Callback(103) = "UpdateLightMap Layer_5_LM_Gi_l103, 100.0, " ' VLM.Lampz;Gi-l103
	Lampz.Callback(103) = "UpdateLightMap Div_LM_Gi_l103, 100.0, " ' VLM.Lampz;Gi-l103
	Lampz.Callback(103) = "UpdateLightMap Iceman_LM_Gi_l103, 100.0, " ' VLM.Lampz;Gi-l103
	Lampz.Callback(103) = "UpdateLightMap Sentinel_LM_Gi_l103, 100.0, " ' VLM.Lampz;Gi-l103
	Lampz.MassAssign(103) = l103b ' VLM.Lampz;Gi-l103b
	Lampz.Callback(103) = "UpdateLightMap Playfield_LM_Gi_l103b, 100.0, " ' VLM.Lampz;Gi-l103b
	Lampz.Callback(103) = "UpdateLightMap REMK_LM_Gi_l103b, 100.0, " ' VLM.Lampz;Gi-l103b
	Lampz.Callback(103) = "UpdateLightMap RFLogo1_LM_Gi_l103b, 100.0, " ' VLM.Lampz;Gi-l103b
	Lampz.Callback(103) = "UpdateLightMap RSling1_LM_Gi_l103b, 100.0, " ' VLM.Lampz;Gi-l103b
	Lampz.Callback(103) = "UpdateLightMap RSling2_LM_Gi_l103b, 100.0, " ' VLM.Lampz;Gi-l103b
	Lampz.Callback(103) = "UpdateLightMap Iceman_LM_Gi_l103b, 100.0, " ' VLM.Lampz;Gi-l103b
	Lampz.Callback(103) = "UpdateLightMap Wolverine_LM_Gi_l103b, 100.0, " ' VLM.Lampz;Gi-l103b
	Lampz.Callback(103) = "UpdateLightMap NCL_Top_LM_Gi_l103b, 100.0, " ' VLM.Lampz;Gi-l103b



	' Lampz.MassAssign(117) = f17 ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Playfield_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Layer_1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Layer_3_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Layer_4_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Layer_5_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap BR1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap BR2_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap BR3_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Disc_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap LFLogo_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap OutPostL_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap RSling2_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap RampGate1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap SpinnerT2_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw1p_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw24_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw25_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw28_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw2p_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Iceman_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Wolverine_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap ApronMag_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap BBird_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap NCR_Top_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.MassAssign(117) = f17a
	Lampz.MassAssign(117) = f17b
	Lampz.MassAssign(118) = f18 ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Playfield_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Layer_1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Layer_3_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Layer_4_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Layer_5_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap BR1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap BR2_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap BR3_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Disc_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap LSling1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap LSling2_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap RFLogo1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap RSling2_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap RampGate3_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap RampGate4_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap SpinnerT2_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw28_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw49_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw53_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap uppostp_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Iceman_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Wolverine_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap ApronMag_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap BBird_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Sentinel_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap NCR_Top_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	' Lampz.MassAssign(119) = f19 ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Playfield_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Layer_1_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Layer_3_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap BR1_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Disc_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap RFLogo1_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap SpinnerT2_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Iceman_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Wolverine_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap NCL_Bot_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap NCL_Top_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap NCR_Bot_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap NCR_Top_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	' Lampz.MassAssign(120) = f20 ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Playfield_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Layer_3_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap BR1_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Disc_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap RFLogo1_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap SpinnerT2_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Iceman_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Wolverine_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap NCL_Bot_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap NCL_Top_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap NCR_Bot_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	' Lampz.MassAssign(121) = f21 ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap Playfield_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap Layer_4_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap BR2_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap BR3_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap LEMK_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap LFLogo_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap LSling1_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap LSling2_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap OutPostR_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap REMK_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap RFLogo_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap RFLogo1_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap RSling1_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap RSling2_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap sw1p_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap sw28_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap sw29_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap sw2p_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap sw33_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap sw8p_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap Iceman_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap Wolverine_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap ApronMag_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap NCL_Top_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	Lampz.Callback(121) = "UpdateLightMap NCR_Top_LM_Flashers_f21, 100.0, " ' VLM.Lampz;Flashers-f21
	' Lampz.MassAssign(122) = f22 ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Playfield_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Layer_1_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Layer_2_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Layer_3_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Layer_4_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Layer_5_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap BR1_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Div_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap SpinnerT2_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Target_001_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Target_004_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap sw53_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap sw54_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap uppostp_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Wolverine_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap Sentinel_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap NCL_Top_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.Callback(122) = "UpdateLightMap NCR_Top_LM_Flashers_f22, 100.0, " ' VLM.Lampz;Flashers-f22
	Lampz.MassAssign(125) = f25 ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap Playfield_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap Layer_1_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap Layer_3_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap Layer_4_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap Layer_5_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap BR1_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap BR2_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap BR3_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap RFLogo1_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap RampGate1_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap Iceman_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap Wolverine_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap BBird_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.Callback(125) = "UpdateLightMap NCL_Top_LM_Flashers_f25, 100.0, " ' VLM.Lampz;Flashers-f25
	Lampz.MassAssign(128) = f28 ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Playfield_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Layer_1_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Layer_2_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Layer_3_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Layer_4_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Layer_5_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap SpinnerT2_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.MassAssign(129) = f29 ' VLM.Lampz;Flashers-f29
	Lampz.Callback(129) = "UpdateLightMap Playfield_LM_Flashers_f29, 100.0, " ' VLM.Lampz;Flashers-f29
	Lampz.Callback(129) = "UpdateLightMap Layer_1_LM_Flashers_f29, 100.0, " ' VLM.Lampz;Flashers-f29
	Lampz.Callback(129) = "UpdateLightMap Layer_2_LM_Flashers_f29, 100.0, " ' VLM.Lampz;Flashers-f29
	Lampz.Callback(129) = "UpdateLightMap Layer_3_LM_Flashers_f29, 100.0, " ' VLM.Lampz;Flashers-f29
	Lampz.Callback(129) = "UpdateLightMap Layer_5_LM_Flashers_f29, 100.0, " ' VLM.Lampz;Flashers-f29
	Lampz.Callback(129) = "UpdateLightMap Div_LM_Flashers_f29, 100.0, " ' VLM.Lampz;Flashers-f29
	' Lampz.MassAssign(130) = f30 ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap Playfield_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap Layer_1_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap Layer_3_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap Layer_4_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap Layer_5_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap SpinnerT2_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap Target_001_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap Target_004_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap sw53_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap uppostp_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap NCL_Bot_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap NCL_Top_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	Lampz.Callback(130) = "UpdateLightMap NCR_Top_LM_Flashers_f30, 100.0, " ' VLM.Lampz;Flashers-f30
	' Lampz.MassAssign(131) = f31 ' VLM.Lampz;Flashers-f31
	Lampz.Callback(131) = "UpdateLightMap Playfield_LM_Flashers_f31, 100.0, " ' VLM.Lampz;Flashers-f31
	Lampz.Callback(131) = "UpdateLightMap ApronCol_LM_Flashers_f31, 300.0, " ' VLM.Lampz;Flashers-f31
	' Lampz.MassAssign(132) = f32 ' VLM.Lampz;Flashers-f32
	Lampz.Callback(132) = "UpdateLightMap Playfield_LM_Flashers_f32, 100.0, " ' VLM.Lampz;Flashers-f32

	Lampz.MassAssign(17) = L17 ' VLM.Lampz;Inserts-L17
	Lampz.Callback(17) = "UpdateLightMap Playfield_LM_Inserts_L17, 100.0, " ' VLM.Lampz;Inserts-L17
	Lampz.Callback(17) = "UpdateLightMap LFLogo_LM_Inserts_L17, 100.0, " ' VLM.Lampz;Inserts-L17
	Lampz.Callback(17) = "UpdateLightMap REMK_LM_Inserts_L17, 100.0, " ' VLM.Lampz;Inserts-L17
	Lampz.Callback(17) = "UpdateLightMap RFLogo_LM_Inserts_L17, 100.0, " ' VLM.Lampz;Inserts-L17
	Lampz.MassAssign(18) = L18 ' VLM.Lampz;Inserts-L18
	Lampz.Callback(18) = "UpdateLightMap Playfield_LM_Inserts_L18, 100.0, " ' VLM.Lampz;Inserts-L18
	Lampz.Callback(18) = "UpdateLightMap LFLogo_LM_Inserts_L18, 100.0, " ' VLM.Lampz;Inserts-L18
	Lampz.Callback(18) = "UpdateLightMap RFLogo_LM_Inserts_L18, 100.0, " ' VLM.Lampz;Inserts-L18
	Lampz.MassAssign(19) = L19 ' VLM.Lampz;Inserts-L19
	Lampz.Callback(19) = "UpdateLightMap Playfield_LM_Inserts_L19, 100.0, " ' VLM.Lampz;Inserts-L19
	Lampz.Callback(19) = "UpdateLightMap LFLogo_LM_Inserts_L19, 100.0, " ' VLM.Lampz;Inserts-L19
	Lampz.Callback(19) = "UpdateLightMap RFLogo_LM_Inserts_L19, 100.0, " ' VLM.Lampz;Inserts-L19
	Lampz.MassAssign(20) = L20 ' VLM.Lampz;Inserts-L20
	Lampz.Callback(20) = "UpdateLightMap Playfield_LM_Inserts_L20, 100.0, " ' VLM.Lampz;Inserts-L20
	Lampz.MassAssign(21) = L21 ' VLM.Lampz;Inserts-L21
	Lampz.Callback(21) = "UpdateLightMap Playfield_LM_Inserts_L21, 100.0, " ' VLM.Lampz;Inserts-L21
	Lampz.MassAssign(22) = L22 ' VLM.Lampz;Inserts-L22
	Lampz.Callback(22) = "UpdateLightMap Playfield_LM_Inserts_L22, 100.0, " ' VLM.Lampz;Inserts-L22
	Lampz.MassAssign(23) = L23 ' VLM.Lampz;Inserts-L23
	Lampz.Callback(23) = "UpdateLightMap Playfield_LM_Inserts_L23, 100.0, " ' VLM.Lampz;Inserts-L23
	Lampz.Callback(23) = "UpdateLightMap Iceman_LM_Inserts_L23, 100.0, " ' VLM.Lampz;Inserts-L23
	Lampz.MassAssign(24) = L24 ' VLM.Lampz;Inserts-L24
	Lampz.Callback(24) = "UpdateLightMap Playfield_LM_Inserts_L24, 100.0, " ' VLM.Lampz;Inserts-L24
	Lampz.MassAssign(25) = L25 ' VLM.Lampz;Inserts-L25
	Lampz.Callback(25) = "UpdateLightMap Playfield_LM_Inserts_L25, 100.0, " ' VLM.Lampz;Inserts-L25
	Lampz.MassAssign(26) = L26 ' VLM.Lampz;Inserts-L26
	Lampz.Callback(26) = "UpdateLightMap Playfield_LM_Inserts_L26, 100.0, " ' VLM.Lampz;Inserts-L26
	Lampz.MassAssign(27) = L27 ' VLM.Lampz;Inserts-L27
	Lampz.Callback(27) = "UpdateLightMap Playfield_LM_Inserts_L27, 100.0, " ' VLM.Lampz;Inserts-L27
	Lampz.MassAssign(28) = L28 ' VLM.Lampz;Inserts-L28
	Lampz.Callback(28) = "UpdateLightMap Playfield_LM_Inserts_L28, 100.0, " ' VLM.Lampz;Inserts-L28
	Lampz.Callback(28) = "UpdateLightMap LEMK_LM_Inserts_L28, 100.0, " ' VLM.Lampz;Inserts-L28
	Lampz.Callback(28) = "UpdateLightMap LFLogo_LM_Inserts_L28, 100.0, " ' VLM.Lampz;Inserts-L28
	Lampz.Callback(28) = "UpdateLightMap RFLogo_LM_Inserts_L28, 100.0, " ' VLM.Lampz;Inserts-L28
	Lampz.MassAssign(29) = L29 ' VLM.Lampz;Inserts-L29
	Lampz.Callback(29) = "UpdateLightMap Playfield_LM_Inserts_L29, 100.0, " ' VLM.Lampz;Inserts-L29
	Lampz.Callback(29) = "UpdateLightMap LFLogo_LM_Inserts_L29, 100.0, " ' VLM.Lampz;Inserts-L29
	Lampz.Callback(29) = "UpdateLightMap RFLogo_LM_Inserts_L29, 100.0, " ' VLM.Lampz;Inserts-L29
	Lampz.Callback(29) = "UpdateLightMap ApronMag_LM_Inserts_L29, 100.0, " ' VLM.Lampz;Inserts-L29
	Lampz.MassAssign(30) = L30 ' VLM.Lampz;Inserts-L30
	Lampz.Callback(30) = "UpdateLightMap Playfield_LM_Inserts_L30, 100.0, " ' VLM.Lampz;Inserts-L30
	Lampz.Callback(30) = "UpdateLightMap LFLogo_LM_Inserts_L30, 100.0, " ' VLM.Lampz;Inserts-L30
	Lampz.Callback(30) = "UpdateLightMap RFLogo_LM_Inserts_L30, 100.0, " ' VLM.Lampz;Inserts-L30
	Lampz.MassAssign(31) = L31 ' VLM.Lampz;Inserts-L31
	Lampz.Callback(31) = "UpdateLightMap Playfield_LM_Inserts_L31, 100.0, " ' VLM.Lampz;Inserts-L31
	Lampz.Callback(31) = "UpdateLightMap LFLogo_LM_Inserts_L31, 100.0, " ' VLM.Lampz;Inserts-L31
	Lampz.Callback(31) = "UpdateLightMap RFLogo_LM_Inserts_L31, 100.0, " ' VLM.Lampz;Inserts-L31
	Lampz.MassAssign(32) = L32 ' VLM.Lampz;Inserts-L32
	Lampz.Callback(32) = "UpdateLightMap Playfield_LM_Inserts_L32, 100.0, " ' VLM.Lampz;Inserts-L32
	Lampz.Callback(32) = "UpdateLightMap LFLogo_LM_Inserts_L32, 100.0, " ' VLM.Lampz;Inserts-L32
	Lampz.Callback(32) = "UpdateLightMap RFLogo_LM_Inserts_L32, 100.0, " ' VLM.Lampz;Inserts-L32
	Lampz.MassAssign(33) = L33 ' VLM.Lampz;Inserts-L33
	Lampz.Callback(33) = "UpdateLightMap Playfield_LM_Inserts_L33, 100.0, " ' VLM.Lampz;Inserts-L33
	Lampz.Callback(33) = "UpdateLightMap Layer_4_LM_Inserts_L33, 100.0, " ' VLM.Lampz;Inserts-L33
	Lampz.MassAssign(34) = L34 ' VLM.Lampz;Inserts-L34
	Lampz.Callback(34) = "UpdateLightMap Playfield_LM_Inserts_L34, 100.0, " ' VLM.Lampz;Inserts-L34
	Lampz.Callback(34) = "UpdateLightMap Layer_4_LM_Inserts_L34, 100.0, " ' VLM.Lampz;Inserts-L34
	Lampz.MassAssign(35) = L35 ' VLM.Lampz;Inserts-L35
	Lampz.Callback(35) = "UpdateLightMap Playfield_LM_Inserts_L35, 100.0, " ' VLM.Lampz;Inserts-L35
	Lampz.Callback(35) = "UpdateLightMap Layer_4_LM_Inserts_L35, 100.0, " ' VLM.Lampz;Inserts-L35
	Lampz.Callback(35) = "UpdateLightMap sw1p_LM_Inserts_L35, 100.0, " ' VLM.Lampz;Inserts-L35
	Lampz.Callback(35) = "UpdateLightMap sw2p_LM_Inserts_L35, 100.0, " ' VLM.Lampz;Inserts-L35
	Lampz.MassAssign(36) = L36 ' VLM.Lampz;Inserts-L36
	Lampz.Callback(36) = "UpdateLightMap Playfield_LM_Inserts_L36, 100.0, " ' VLM.Lampz;Inserts-L36
	Lampz.Callback(36) = "UpdateLightMap Layer_4_LM_Inserts_L36, 100.0, " ' VLM.Lampz;Inserts-L36
	Lampz.Callback(36) = "UpdateLightMap sw1p_LM_Inserts_L36, 100.0, " ' VLM.Lampz;Inserts-L36
	Lampz.Callback(36) = "UpdateLightMap sw2p_LM_Inserts_L36, 100.0, " ' VLM.Lampz;Inserts-L36
	Lampz.MassAssign(37) = L37 ' VLM.Lampz;Inserts-L37
	Lampz.Callback(37) = "UpdateLightMap Playfield_LM_Inserts_L37, 100.0, " ' VLM.Lampz;Inserts-L37
	Lampz.Callback(37) = "UpdateLightMap Wolverine_LM_Inserts_L37, 100.0, " ' VLM.Lampz;Inserts-L37
	Lampz.MassAssign(38) = L38 ' VLM.Lampz;Inserts-L38
	Lampz.Callback(38) = "UpdateLightMap Playfield_LM_Inserts_L38, 100.0, " ' VLM.Lampz;Inserts-L38
	Lampz.Callback(38) = "UpdateLightMap Wolverine_LM_Inserts_L38, 100.0, " ' VLM.Lampz;Inserts-L38
	Lampz.MassAssign(41) = L41 ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap Playfield_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap Layer_1_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap Layer_4_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap Layer_5_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap BR1_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap BR2_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap BR3_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap Iceman_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap Wolverine_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.Callback(41) = "UpdateLightMap BBird_LM_Inserts_L41, 100.0, " ' VLM.Lampz;Inserts-L41
	Lampz.MassAssign(42) = L42 ' VLM.Lampz;Inserts-L42
	Lampz.Callback(42) = "UpdateLightMap Playfield_LM_Inserts_L42, 100.0, " ' VLM.Lampz;Inserts-L42
	Lampz.Callback(42) = "UpdateLightMap BR1_LM_Inserts_L42, 100.0, " ' VLM.Lampz;Inserts-L42
	Lampz.Callback(42) = "UpdateLightMap BR2_LM_Inserts_L42, 100.0, " ' VLM.Lampz;Inserts-L42
	Lampz.Callback(42) = "UpdateLightMap BR3_LM_Inserts_L42, 100.0, " ' VLM.Lampz;Inserts-L42
	Lampz.Callback(42) = "UpdateLightMap SpinnerT2_LM_Inserts_L42, 100.0, " ' VLM.Lampz;Inserts-L42
	Lampz.Callback(42) = "UpdateLightMap Wolverine_LM_Inserts_L42, 100.0, " ' VLM.Lampz;Inserts-L42
	Lampz.Callback(42) = "UpdateLightMap BBird_LM_Inserts_L42, 100.0, " ' VLM.Lampz;Inserts-L42
	Lampz.Callback(42) = "UpdateLightMap NCL_Top_LM_Inserts_L42, 100.0, " ' VLM.Lampz;Inserts-L42
	Lampz.MassAssign(43) = L43 ' VLM.Lampz;Inserts-L43
	Lampz.Callback(43) = "UpdateLightMap Playfield_LM_Inserts_L43, 100.0, " ' VLM.Lampz;Inserts-L43
	Lampz.Callback(43) = "UpdateLightMap Layer_1_LM_Inserts_L43, 100.0, " ' VLM.Lampz;Inserts-L43
	Lampz.Callback(43) = "UpdateLightMap Layer_4_LM_Inserts_L43, 100.0, " ' VLM.Lampz;Inserts-L43
	Lampz.Callback(43) = "UpdateLightMap BR2_LM_Inserts_L43, 100.0, " ' VLM.Lampz;Inserts-L43
	Lampz.Callback(43) = "UpdateLightMap BR3_LM_Inserts_L43, 100.0, " ' VLM.Lampz;Inserts-L43
	Lampz.Callback(43) = "UpdateLightMap RampGate1_LM_Inserts_L43, 100.0, " ' VLM.Lampz;Inserts-L43
	Lampz.MassAssign(45) = L45 ' VLM.Lampz;Inserts-L45
	Lampz.Callback(45) = "UpdateLightMap Playfield_LM_Inserts_L45, 100.0, " ' VLM.Lampz;Inserts-L45
	Lampz.Callback(45) = "UpdateLightMap Wolverine_LM_Inserts_L45, 100.0, " ' VLM.Lampz;Inserts-L45
	Lampz.MassAssign(46) = L46 ' VLM.Lampz;Inserts-L46
	Lampz.Callback(46) = "UpdateLightMap Playfield_LM_Inserts_L46, 100.0, " ' VLM.Lampz;Inserts-L46
	Lampz.Callback(46) = "UpdateLightMap Wolverine_LM_Inserts_L46, 100.0, " ' VLM.Lampz;Inserts-L46
	Lampz.MassAssign(47) = L47 ' VLM.Lampz;Inserts-L47
	Lampz.Callback(47) = "UpdateLightMap Playfield_LM_Inserts_L47, 100.0, " ' VLM.Lampz;Inserts-L47
	Lampz.Callback(47) = "UpdateLightMap Iceman_LM_Inserts_L47, 100.0, " ' VLM.Lampz;Inserts-L47
	Lampz.MassAssign(48) = L48 ' VLM.Lampz;Inserts-L48
	Lampz.Callback(48) = "UpdateLightMap Playfield_LM_Inserts_L48, 100.0, " ' VLM.Lampz;Inserts-L48
	Lampz.MassAssign(49) = L49 ' VLM.Lampz;Inserts-L49
	Lampz.Callback(49) = "UpdateLightMap Playfield_LM_Inserts_L49, 100.0, " ' VLM.Lampz;Inserts-L49
	Lampz.Callback(49) = "UpdateLightMap Layer_3_LM_Inserts_L49, 100.0, " ' VLM.Lampz;Inserts-L49
	Lampz.Callback(49) = "UpdateLightMap Layer_4_LM_Inserts_L49, 100.0, " ' VLM.Lampz;Inserts-L49
	Lampz.MassAssign(50) = L50 ' VLM.Lampz;Inserts-L50
	Lampz.Callback(50) = "UpdateLightMap Playfield_LM_Inserts_L50, 100.0, " ' VLM.Lampz;Inserts-L50
	Lampz.Callback(50) = "UpdateLightMap Layer_3_LM_Inserts_L50, 100.0, " ' VLM.Lampz;Inserts-L50
	Lampz.MassAssign(51) = L51 ' VLM.Lampz;Inserts-L51
	Lampz.Callback(51) = "UpdateLightMap Playfield_LM_Inserts_L51, 100.0, " ' VLM.Lampz;Inserts-L51
	Lampz.Callback(51) = "UpdateLightMap Layer_3_LM_Inserts_L51, 100.0, " ' VLM.Lampz;Inserts-L51
	Lampz.Callback(51) = "UpdateLightMap Layer_4_LM_Inserts_L51, 100.0, " ' VLM.Lampz;Inserts-L51
	Lampz.MassAssign(52) = L52 ' VLM.Lampz;Inserts-L52
	Lampz.Callback(52) = "UpdateLightMap Playfield_LM_Inserts_L52, 100.0, " ' VLM.Lampz;Inserts-L52
	Lampz.Callback(52) = "UpdateLightMap BR1_LM_Inserts_L52, 100.0, " ' VLM.Lampz;Inserts-L52
	Lampz.Callback(52) = "UpdateLightMap BR3_LM_Inserts_L52, 100.0, " ' VLM.Lampz;Inserts-L52
	Lampz.Callback(52) = "UpdateLightMap Wolverine_LM_Inserts_L52, 100.0, " ' VLM.Lampz;Inserts-L52
	Lampz.MassAssign(53) = L53 ' VLM.Lampz;Inserts-L53
	Lampz.Callback(53) = "UpdateLightMap Playfield_LM_Inserts_L53, 100.0, " ' VLM.Lampz;Inserts-L53
	Lampz.Callback(53) = "UpdateLightMap Layer_4_LM_Inserts_L53, 100.0, " ' VLM.Lampz;Inserts-L53
	Lampz.MassAssign(54) = L54 ' VLM.Lampz;Inserts-L54
	Lampz.Callback(54) = "UpdateLightMap Playfield_LM_Inserts_L54, 100.0, " ' VLM.Lampz;Inserts-L54
	Lampz.Callback(54) = "UpdateLightMap Wolverine_LM_Inserts_L54, 100.0, " ' VLM.Lampz;Inserts-L54
	Lampz.MassAssign(55) = L55 ' VLM.Lampz;Inserts-L55
	Lampz.Callback(55) = "UpdateLightMap Playfield_LM_Inserts_L55, 100.0, " ' VLM.Lampz;Inserts-L55
	Lampz.Callback(55) = "UpdateLightMap Layer_3_LM_Inserts_L55, 100.0, " ' VLM.Lampz;Inserts-L55
	Lampz.Callback(55) = "UpdateLightMap Layer_4_LM_Inserts_L55, 100.0, " ' VLM.Lampz;Inserts-L55
	Lampz.MassAssign(56) = L56 ' VLM.Lampz;Inserts-L56
	Lampz.Callback(56) = "UpdateLightMap Playfield_LM_Inserts_L56, 100.0, " ' VLM.Lampz;Inserts-L56
	Lampz.Callback(56) = "UpdateLightMap Layer_3_LM_Inserts_L56, 100.0, " ' VLM.Lampz;Inserts-L56
	Lampz.Callback(56) = "UpdateLightMap Layer_4_LM_Inserts_L56, 100.0, " ' VLM.Lampz;Inserts-L56
	Lampz.MassAssign(57) = L57 ' VLM.Lampz;Inserts-L57
	Lampz.Callback(57) = "UpdateLightMap Playfield_LM_Inserts_L57, 100.0, " ' VLM.Lampz;Inserts-L57
	Lampz.Callback(57) = "UpdateLightMap Layer_4_LM_Inserts_L57, 100.0, " ' VLM.Lampz;Inserts-L57
	Lampz.MassAssign(58) = L58 ' VLM.Lampz;Inserts-L58
	Lampz.Callback(58) = "UpdateLightMap Playfield_LM_Inserts_L58, 100.0, " ' VLM.Lampz;Inserts-L58
	Lampz.Callback(58) = "UpdateLightMap Layer_4_LM_Inserts_L58, 100.0, " ' VLM.Lampz;Inserts-L58
	Lampz.MassAssign(59) = L59 ' VLM.Lampz;Inserts-L59
	Lampz.Callback(59) = "UpdateLightMap Playfield_LM_Inserts_L59, 100.0, " ' VLM.Lampz;Inserts-L59
	Lampz.Callback(59) = "UpdateLightMap Layer_1_LM_Inserts_L59, 100.0, " ' VLM.Lampz;Inserts-L59
	Lampz.Callback(59) = "UpdateLightMap Layer_4_LM_Inserts_L59, 100.0, " ' VLM.Lampz;Inserts-L59
	Lampz.Callback(59) = "UpdateLightMap Iceman_LM_Inserts_L59, 100.0, " ' VLM.Lampz;Inserts-L59
	Lampz.Callback(59) = "UpdateLightMap Wolverine_LM_Inserts_L59, 100.0, " ' VLM.Lampz;Inserts-L59
	Lampz.MassAssign(60) = L60 ' VLM.Lampz;Inserts-L60
	Lampz.Callback(60) = "UpdateLightMap Playfield_LM_Inserts_L60, 100.0, " ' VLM.Lampz;Inserts-L60
	Lampz.Callback(60) = "UpdateLightMap Layer_4_LM_Inserts_L60, 100.0, " ' VLM.Lampz;Inserts-L60
	Lampz.Callback(60) = "UpdateLightMap Target_004_LM_Inserts_L60, 100.0, " ' VLM.Lampz;Inserts-L60
	Lampz.Callback(60) = "UpdateLightMap NCL_Top_LM_Inserts_L60, 100.0, " ' VLM.Lampz;Inserts-L60
	Lampz.MassAssign(65) = L65 ' VLM.Lampz;Inserts-L65
	Lampz.Callback(65) = "UpdateLightMap Playfield_LM_Inserts_L65, 100.0, " ' VLM.Lampz;Inserts-L65
	Lampz.Callback(65) = "UpdateLightMap BR1_LM_Inserts_L65, 100.0, " ' VLM.Lampz;Inserts-L65
	Lampz.Callback(65) = "UpdateLightMap Wolverine_LM_Inserts_L65, 100.0, " ' VLM.Lampz;Inserts-L65
	Lampz.MassAssign(66) = L66 ' VLM.Lampz;Inserts-L66
	Lampz.Callback(66) = "UpdateLightMap Playfield_LM_Inserts_L66, 100.0, " ' VLM.Lampz;Inserts-L66
	Lampz.Callback(66) = "UpdateLightMap RFLogo1_LM_Inserts_L66, 100.0, " ' VLM.Lampz;Inserts-L66
	Lampz.Callback(66) = "UpdateLightMap SpinnerT2_LM_Inserts_L66, 100.0, " ' VLM.Lampz;Inserts-L66
	Lampz.MassAssign(67) = L67 ' VLM.Lampz;Inserts-L67
	Lampz.Callback(67) = "UpdateLightMap Playfield_LM_Inserts_L67, 100.0, " ' VLM.Lampz;Inserts-L67
	Lampz.Callback(67) = "UpdateLightMap Layer_4_LM_Inserts_L67, 100.0, " ' VLM.Lampz;Inserts-L67
	Lampz.Callback(67) = "UpdateLightMap uppostp_LM_Inserts_L67, 100.0, " ' VLM.Lampz;Inserts-L67
	Lampz.Callback(67) = "UpdateLightMap NCL_Bot_LM_Inserts_L67, 100.0, " ' VLM.Lampz;Inserts-L67
	Lampz.MassAssign(68) = L68 ' VLM.Lampz;Inserts-L68
	Lampz.Callback(68) = "UpdateLightMap Playfield_LM_Inserts_L68, 100.0, " ' VLM.Lampz;Inserts-L68
	Lampz.MassAssign(69) = L69 ' VLM.Lampz;Inserts-L69
	Lampz.Callback(69) = "UpdateLightMap Playfield_LM_Inserts_L69, 100.0, " ' VLM.Lampz;Inserts-L69
	Lampz.Callback(69) = "UpdateLightMap OutPostR_LM_Inserts_L69, 100.0, " ' VLM.Lampz;Inserts-L69
	Lampz.MassAssign(70) = L70 ' VLM.Lampz;Inserts-L70
	Lampz.Callback(70) = "UpdateLightMap Playfield_LM_Inserts_L70, 100.0, " ' VLM.Lampz;Inserts-L70
	Lampz.MassAssign(71) = L71 ' VLM.Lampz;Inserts-L71
	Lampz.Callback(71) = "UpdateLightMap Playfield_LM_Inserts_L71, 100.0, " ' VLM.Lampz;Inserts-L71
	Lampz.Callback(71) = "UpdateLightMap RFLogo1_LM_Inserts_L71, 100.0, " ' VLM.Lampz;Inserts-L71
	Lampz.Callback(71) = "UpdateLightMap sw7p_LM_Inserts_L71, 100.0, " ' VLM.Lampz;Inserts-L71
	Lampz.Callback(71) = "UpdateLightMap sw8p_LM_Inserts_L71, 100.0, " ' VLM.Lampz;Inserts-L71
	Lampz.Callback(71) = "UpdateLightMap Iceman_LM_Inserts_L71, 100.0, " ' VLM.Lampz;Inserts-L71
	Lampz.MassAssign(72) = L72 ' VLM.Lampz;Inserts-L72
	Lampz.Callback(72) = "UpdateLightMap Playfield_LM_Inserts_L72, 100.0, " ' VLM.Lampz;Inserts-L72
	Lampz.Callback(72) = "UpdateLightMap RFLogo1_LM_Inserts_L72, 100.0, " ' VLM.Lampz;Inserts-L72
	Lampz.Callback(72) = "UpdateLightMap sw7p_LM_Inserts_L72, 100.0, " ' VLM.Lampz;Inserts-L72
	Lampz.Callback(72) = "UpdateLightMap sw8p_LM_Inserts_L72, 100.0, " ' VLM.Lampz;Inserts-L72
	Lampz.MassAssign(73) = L73 ' VLM.Lampz;Inserts-L73
	Lampz.Callback(73) = "UpdateLightMap Playfield_LM_Inserts_L73, 100.0, " ' VLM.Lampz;Inserts-L73
	Lampz.Callback(73) = "UpdateLightMap Layer_3_LM_Inserts_L73, 100.0, " ' VLM.Lampz;Inserts-L73
	Lampz.Callback(73) = "UpdateLightMap Layer_4_LM_Inserts_L73, 100.0, " ' VLM.Lampz;Inserts-L73
	Lampz.Callback(73) = "UpdateLightMap SpinnerT2_LM_Inserts_L73, 100.0, " ' VLM.Lampz;Inserts-L73
	Lampz.MassAssign(74) = L74 ' VLM.Lampz;Inserts-L74
	Lampz.Callback(74) = "UpdateLightMap Playfield_LM_Inserts_L74, 100.0, " ' VLM.Lampz;Inserts-L74
	Lampz.Callback(74) = "UpdateLightMap Layer_3_LM_Inserts_L74, 100.0, " ' VLM.Lampz;Inserts-L74
	Lampz.MassAssign(75) = L75 ' VLM.Lampz;Inserts-L75
	Lampz.Callback(75) = "UpdateLightMap Playfield_LM_Inserts_L75, 100.0, " ' VLM.Lampz;Inserts-L75
	Lampz.Callback(75) = "UpdateLightMap Layer_4_LM_Inserts_L75, 100.0, " ' VLM.Lampz;Inserts-L75
	Lampz.Callback(75) = "UpdateLightMap SpinnerT2_LM_Inserts_L75, 100.0, " ' VLM.Lampz;Inserts-L75
	Lampz.MassAssign(76) = L76 ' VLM.Lampz;Inserts-L76
	Lampz.Callback(76) = "UpdateLightMap Playfield_LM_Inserts_L76, 100.0, " ' VLM.Lampz;Inserts-L76
	Lampz.Callback(76) = "UpdateLightMap Layer_4_LM_Inserts_L76, 100.0, " ' VLM.Lampz;Inserts-L76
	Lampz.Callback(76) = "UpdateLightMap SpinnerT2_LM_Inserts_L76, 100.0, " ' VLM.Lampz;Inserts-L76
	Lampz.MassAssign(77) = L77 ' VLM.Lampz;Inserts-L77
	Lampz.Callback(77) = "UpdateLightMap Playfield_LM_Inserts_L77, 100.0, " ' VLM.Lampz;Inserts-L77
	Lampz.Callback(77) = "UpdateLightMap SpinnerT2_LM_Inserts_L77, 100.0, " ' VLM.Lampz;Inserts-L77
	Lampz.Callback(77) = "UpdateLightMap Target_001_LM_Inserts_L77, 100.0, " ' VLM.Lampz;Inserts-L77
	Lampz.MassAssign(78) = L78 ' VLM.Lampz;Inserts-L78
	Lampz.Callback(78) = "UpdateLightMap Playfield_LM_Inserts_L78, 100.0, " ' VLM.Lampz;Inserts-L78
	Lampz.Callback(78) = "UpdateLightMap Layer_1_LM_Inserts_L78, 100.0, " ' VLM.Lampz;Inserts-L78
	Lampz.Callback(78) = "UpdateLightMap Layer_3_LM_Inserts_L78, 100.0, " ' VLM.Lampz;Inserts-L78
	Lampz.Callback(78) = "UpdateLightMap Layer_4_LM_Inserts_L78, 100.0, " ' VLM.Lampz;Inserts-L78
	Lampz.MassAssign(79) = L79 ' VLM.Lampz;Inserts-L79
	Lampz.Callback(79) = "UpdateLightMap Playfield_LM_Inserts_L79, 100.0, " ' VLM.Lampz;Inserts-L79
	Lampz.Callback(79) = "UpdateLightMap Layer_1_LM_Inserts_L79, 100.0, " ' VLM.Lampz;Inserts-L79
	Lampz.Callback(79) = "UpdateLightMap Layer_3_LM_Inserts_L79, 100.0, " ' VLM.Lampz;Inserts-L79
	Lampz.Callback(79) = "UpdateLightMap Layer_4_LM_Inserts_L79, 100.0, " ' VLM.Lampz;Inserts-L79
	Lampz.MassAssign(80) = L80 ' VLM.Lampz;Inserts-L80
	Lampz.Callback(80) = "UpdateLightMap Playfield_LM_Inserts_L80, 100.0, " ' VLM.Lampz;Inserts-L80
	Lampz.Callback(80) = "UpdateLightMap Layer_3_LM_Inserts_L80, 100.0, " ' VLM.Lampz;Inserts-L80
	Lampz.Callback(80) = "UpdateLightMap Iceman_LM_Inserts_L80, 100.0, " ' VLM.Lampz;Inserts-L80

	' Backglass flashers

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update
End Sub


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

Const LiveCatch = 16

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

dim LFPress, RFPress, RFPress1, LFCount, RFCount
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

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

Sub zCol_Rubber_Post_RS3_Hit 
	RubbersD.dampen Activeball
End Sub

Sub zCol_Rubber_Post_LS3_Hit 
	RubbersD.dampen Activeball
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
		dim str, b, AllBalls, highestID : allBalls = getballs

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

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

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

'****************************************************************
'	END nFozzy Physics
'****************************************************************



'*********************************************************************
' Narnia Catcher
'*********************************************************************

Sub Narnia_Timer
	Dim b : For b = 0 to UBound(gBOT)
		'Check for narnia balls
		If gBOT(b).z < -200 Then
			'msgbox "Ball " &b& " in Narnia X: " & gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z
			'debug.print "Move narnia ball ("& gBOT(b).x &" Y: "&gBOT(b).y & " Z: "&gBOT(b).z&") to scoop"
			gBOT(b).x = 195 : gBOT(b).y = 1072 : gBOT(b).z = -30
			gBOT(b).velx = 0 : gBOT(b).vely = 0 : gBOT(b).velz = 0
		end if
	Next
End Sub





'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iaakki, Apophis, and Wylte
'***************************************************************

Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(4), objrtx2(4)
dim objBallShadow(4)
dim objSpotShadow1(4), objSpotShadow2(4)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.51			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.52
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.54
		objBallShadow(iii).visible = 0

		Set objSpotShadow1(iii) = Eval("SpotlightShadow" & iii)
		objSpotShadow1(iii).material = "BallSpotShadow" & iii
		objSpotShadow1(iii).Z = 1 + iii/1000 + 0.56
		objSpotShadow1(iii).visible = 0
		Set objSpotShadow2(iii) = Eval("Spotlight2Shadow" & iii)
		objSpotShadow2(iii).material = "BallSpot2Shadow" & iii
		objSpotShadow2(iii).Z = 1 + iii/1000 + 0.97
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

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 5		'Offset y position under ball	 (^^for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

Const falloff   			= 150 	'Max distance to light sources, can be changed if you have a reason
Dim GIfalloff : GIfalloff   = 250
Const Spotfalloff 			= 200

Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
		BallShadowA(num).visible = 1
	End If
End Sub

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
	Dim d_r, d_b, d_w, d_w1
	Dim b_base, b_r, b_g, b_b
	b_base = 16 + (192 - 64) * (LightLevel / 100)
	For s = lob to UBound(gBOT)

' *** Compute ball lighting from GI and ambient lighting
		d_w1 = GIfalloff
		For Each l in GI_Red
			LSd = Distance(gBOT(s).x, gBOT(s).y, l.x, l.y) 'Calculating the Linear distance to the Source
			If LSd < d_w1 then d_w1 = LSd
		Next
		d_w1 = 48 * (1 - d_w1 / GIfalloff) * 0.01 * Playfield_LM_Gi_l103.Opacity
		d_r = GIfalloff
		For Each l in GI_Red
			LSd = Distance(gBOT(s).x, gBOT(s).y, l.x, l.y) 'Calculating the Linear distance to the Source
			If LSd < d_r then d_r = LSd
		Next
		d_r = 128 * (1 - d_r / GIfalloff) * 0.01 * Playfield_LM_Gi_Red_l101a.Opacity
		d_b = GIfalloff
		For Each l in GI_Blue
			LSd = Distance(gBOT(s).x, gBOT(s).y, l.x, l.y) 'Calculating the Linear distance to the Source
			If LSd < d_b then d_b = LSd
		Next
		d_b = 64 * (1 - d_b / GIfalloff) * 0.01 * Playfield_LM_Gi_Blue_l102a.Opacity
		d_w = GIfalloff
		For Each l in GI_White
			LSd = Distance(gBOT(s).x, gBOT(s).y, l.x, l.y) 'Calculating the Linear distance to the Source
			If LSd < d_w then d_w = LSd
		Next
		d_w = b_base + d_w1 + 64 * (1 - d_w / GIfalloff) * 0.01 * Playfield_LM_Gi_White_l100a.Opacity
		b_r = Int(d_w + d_r)
		b_g = Int(d_w + d_b)
		b_b = Int(d_w + d_b * 2)
		If b_r > 255 Then b_r = 255
		If b_g > 255 Then b_g = 255
		If b_b > 255 Then b_b = 255
		gBot(s).color = b_r + (b_g * 256) + (b_b * 256 * 256)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)

	'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update

				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize/10
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp

				objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				objBallShadow(s).Y = gBOT(s).Y + offsetY
				objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
				objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
				UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0

			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s

				objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf

			Else												'Under pf, flasher shadow
				If OnPF(s) Then BallOnPlayfieldNow False, s
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000
			end if

	'Flasher shadow everywhere
		Elseif AmbientBallShadowOn = 2 Then
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize/10
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height= 1.04 + s/1000
			Else											'Under pf
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000
			End If
		End If

' *** Spotlight Shadows
		If SpotlightShadowsOn = 1 Then
			If InRect(gBOT(s).x,gBOT(s).y,179,1339,137,1113,336,1096,431,1343) Then 	'Defining where the spotlight is in effect
				objSpotShadow2(s).visible = 0
				LSd1=Distance(gBOT(s).x, gBOT(s).y, l103a.x, l103a.y)
				If LSd1 < Spotfalloff And l103a.state=1 Then
					objSpotShadow1(s).visible = 1 : objSpotShadow1(s).X = gBOT(s).X : objSpotShadow1(s).Y = gBOT(s).Y + offsetY
					objSpotShadow1(s).rotz = AnglePP(l103a.x, l103a.y, gBOT(s).X, gBOT(s).Y) + 90	'Had to use custom coordinates, light and primitive are shifted
					objSpotShadow1(s).size_y = Wideness/2
					ShadowOpacity = (Spotfalloff-LSd1)/Spotfalloff			'Sets opacity/darkness of shadow by distance to light
					UpdateMaterial objSpotShadow1(s).material,1,0,0,0,0,0,0.5*ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0

				Else
					objSpotShadow1(s).visible = 0
				End If

'			ElseIf InRect(gBOT(s).x,gBOT(s).y,597,1595,465,1288,628,1198,760,1318) Then 	'Defining where the spotlight is in effect
			ElseIf InRect(gBOT(s).x,gBOT(s).y,553,1511,346,1411,475,1236,691,1305) Then 	'Defining where the spotlight is in effect
				objSpotShadow1(s).visible = 0
				LSd2=Distance(gBOT(s).x, gBOT(s).y, l103b.x, l103b.y)
				If LSd2 < Spotfalloff And l103b.state=1 Then
					objSpotShadow2(s).visible = 1 : objSpotShadow2(s).X = gBOT(s).X : objSpotShadow2(s).Y = gBOT(s).Y + offsetY
					objSpotShadow2(s).rotz = AnglePP(l103b.x, l103b.y, gBOT(s).X, gBOT(s).Y) + 90
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
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff Then' And gilvl > 0 Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y
					'objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + offsetY
					'objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
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
			Else 'Hide dynamic shadows everywhere else, just in case
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
		If AmbientBallShadowOn = 0 Then
			BallShadowA(b).visible = 1
			BallShadowA(b).X = gBOT(b).X + offsetX
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(b).Y = gBOT(b).Y + offsetY + BallSize/10
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 1.04 + b/1000
				BallShadowA(b).Y = gBOT(b).Y + offsetY
			End If
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
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.045/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.065/5											'volume multiplier; must not be zero
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

'Sub SoundSaucerKick(saucer)
'	PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
'End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	If Not InRect(ball1.x,ball1.y,457,487,457,227,537,227,537,487) Then   'Not under magneto
		Select Case Int(Rnd*7)+1
			Case 1 : snd = "Ball_Collide_1"
			Case 2 : snd = "Ball_Collide_2"
			Case 3 : snd = "Ball_Collide_3"
			Case 4 : snd = "Ball_Collide_4"
			Case 5 : snd = "Ball_Collide_5"
			Case 6 : snd = "Ball_Collide_6"
			Case 7 : snd = "Ball_Collide_7"
		End Select
	'	debug.print "ballball"
		PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
	End If
End Sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

'***********************************************************************
'* TABLE OPTIONS *******************************************************
'***********************************************************************

Dim BlackBirdMod
Dim SentinelMod
Dim MagnetoLE 			    '0 - Wolverine LE (Blue), 1 - Magneto LE (Red)
Dim Cabinetmode				'0 - Siderails On, 1 - Siderails Off
Dim OutPostMod
Dim IncandescentGI : IncandescentGI = 0     '0 - LED GI, 1 - Incandescent GI
Dim StagedFlipperMod
Dim LightLevel : LightLevel = 50
Dim ColorLUT : ColorLUT = 1
Dim VolumeDial : VolumeDial = 0.8			'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5 	'Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 	'Level of ramp rolling volume. Value between 0 and 1
Dim DynamicBallShadowsOn : DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Dim VRRoomChoice : VRRoomChoice = 1              '1 - Minimal Room, 2 - Ultra Room (only applies when using VR headset)

' Base options
Const Opt_Light = 0
Const Opt_LUT = 1
Const Opt_Outpost = 2
Const Opt_Volume = 3
Const Opt_Volume_Ramp = 4
Const Opt_Volume_Ball = 5
Const Opt_Staged_Flipper = 6
' Table mods & toys
Const Opt_Cabinet = 7
Const Opt_IncandescentGI = 8
Const Opt_Sentinel = 9
Const Opt_BlackBird = 10
Const Opt_MagnetoLE = 11
' Advanced options
Const Opt_DynBallShadow = 12
' VR options
Const Opt_VR_Room = 13
' Informations
Const Opt_Info_1 = 14
Const Opt_Info_2 = 15

Const NOptions = 16

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
	OptN.Text = (OptPos+1) & "/" & NOptions
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
	ElseIf OptPos = Opt_Staged_Flipper Then
		OptTop.Text = "STAGED FLIPPER"
		OptBot.Text = Options_OnOffText(StagedFlipperMod)
		SaveValue cGameName, "STAGED", StagedFlipperMod
	ElseIf OptPos = Opt_IncandescentGI Then
		OptTop.Text = "INCANDESCENT GI"
		OptBot.Text = Options_OnOffText(IncandescentGI)
		SaveValue cGameName, "INC_GI", IncandescentGI
	ElseIf OptPos = Opt_Sentinel Then
		OptTop.Text = "SENTINEL"
		OptBot.Text = Options_OnOffText(SentinelMod)
		SaveValue cGameName, "BLACKBIRD", BlackBirdMod
	ElseIf OptPos = Opt_BlackBird Then
		OptTop.Text = "BLACK BIRD"
		OptBot.Text = Options_OnOffText(BlackBirdMod)
		SaveValue cGameName, "BLACKBIRD", BlackBirdMod
	ElseIf OptPos = Opt_Cabinet Then
		OptTop.Text = "CABINET MODE"
		OptBot.Text = Options_OnOffText(CabinetMode)
		SaveValue cGameName, "CABINET", CabinetMode
	ElseIf OptPos = Opt_MagnetoLE Then
		OptTop.Text = "MAGNETO"
		OptBot.Text = Options_OnOffText(MagnetoLE)
		SaveValue cGameName, "MAGNETO", MagnetoLE
	ElseIf OptPos = Opt_VR_Room Then
		OptTop.Text = "VR ROOM"
		If VRRoomChoice = 0 Then OptBot.Text = "MINIMAL"
		If VRRoomChoice = 1 Then OptBot.Text = "ULTRA"
		SaveValue cGameName, "VRROOM", VRRoomChoice
	ElseIf OptPos = Opt_DynBallShadow Then
		OptTop.Text = "DYN. BALL SHADOWS"
		OptBot.Text = Options_OnOffText(DynamicBallShadowsOn)
		SaveValue cGameName, "DYNBALLSH", DynamicBallShadowsOn
	ElseIf OptPos = Opt_Info_1 Then
		OptTop.Text = "VPX " & VersionMajor & "." & VersionMinor & "." & VersionRevision
		OptBot.Text = "X MEN LE " & TableVersion
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
		If OutPostMod < 0 Then OutPostMod = 2
		If OutPostMod > 2 Then OutPostMod = 0
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
	ElseIf OptPos = Opt_IncandescentGI Then
		IncandescentGI = 1 - IncandescentGI
	ElseIf OptPos = Opt_Staged_Flipper Then
		StagedFlipperMod = 1 - StagedFlipperMod
	ElseIf OptPos = Opt_Sentinel Then
		SentinelMod = 1 - SentinelMod
	ElseIf OptPos = Opt_BlackBird Then
		BlackBirdMod = 1 - BlackBirdMod
	ElseIf OptPos = Opt_Cabinet Then
		CabinetMode = 1 - CabinetMode
	ElseIf OptPos = Opt_MagnetoLE Then
		MagnetoLE = 1 - MagnetoLE
	ElseIf OptPos = Opt_VR_Room Then
		VRRoomChoice = VRRoomChoice + 1
		If VRRoomChoice >= 2 Then VRRoomChoice = 1
	ElseIf OptPos = Opt_DynBallShadow Then
		DynamicBallShadowsOn = 1 - DynamicBallShadowsOn
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
			If OptPos = Opt_VR_Room And RenderingMode <> 2 Then OptPos = OptPos - 1 ' Skip VR option in non VR mode
			If OptPos < 0 Then OptPos = NOptions - 1
		ElseIf keycode = RightFlipperKey Then ' Prev / -
			OptPos = OptPos + 1
			If OptPos = Opt_VR_Room And RenderingMode <> 2 Then OptPos = OptPos + 1 ' Skip VR option in non VR mode
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
    x = LoadValue(cGameName, "INC_GI") : If x <> "" Then IncandescentGI = CInt(x) Else IncandescentGI = 0
    x = LoadValue(cGameName, "STAGED") : If x <> "" Then StagedFlipperMod = CInt(x) Else StagedFlipperMod = 0
    x = LoadValue(cGameName, "SENTINEL") : If x <> "" Then SentinelMod = CInt(x) Else SentinelMod = 0
    x = LoadValue(cGameName, "BLACKBIRD") : If x <> "" Then BlackBirdMod = CInt(x) Else BlackBirdMod = 0
    x = LoadValue(cGameName, "CABINET") : If x <> "" Then CabinetMode = CInt(x) Else CabinetMode = 0
    x = LoadValue(cGameName, "MAGNETO") : If x <> "" Then MagnetoLE = CInt(x) Else MagnetoLE = 0
    x = LoadValue(cGameName, "VRROOM") : If x <> "" Then VRRoomChoice = CInt(x) Else VRRoomChoice = 1
    x = LoadValue(cGameName, "DYNBALLSH") : If x <> "" Then DynamicBallShadowsOn = CInt(x) Else DynamicBallShadowsOn = 1
End Sub

Sub UpdateMods
	Dim bake, x, y, enabled

	enabled = SentinelMod
	Sentinel_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Sentinel
	Sentinel_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Sentinel
	Sentinel_LM_Gi_l103.Visible = enabled ' VLM.Props;LM;1;Sentinel
	Sentinel_LM_Gi_Blue_l102h.Visible = enabled ' VLM.Props;LM;1;Sentinel
	Sentinel_LM_Gi_Blue_l102g.Visible = enabled ' VLM.Props;LM;1;Sentinel
	Sentinel_LM_GI_Red_l101g.Visible = enabled ' VLM.Props;LM;1;Sentinel
	Sentinel_LM_GI_Red_l101f.Visible = enabled ' VLM.Props;LM;1;Sentinel
	Sentinel_LM_Gi_White_l100k.Visible = enabled ' VLM.Props;LM;1;Sentinel
	Sentinel_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Sentinel
	Sentinel_LM_Flashers_f22.Visible = enabled ' VLM.Props;LM;1;Sentinel

	enabled = BlackBirdMod
	BBird_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;BBird
	BBird_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;BBird
	BBird_LM_Gi_l103a.Visible = enabled ' VLM.Props;LM;1;BBird
	BBird_LM_Inserts_L41.Visible = enabled ' VLM.Props;LM;1;BBird
	BBird_LM_Inserts_L42.Visible = enabled ' VLM.Props;LM;1;BBird
	BBird_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;BBird
	BBird_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;BBird
	BBird_LM_Flashers_f25.Visible = enabled ' VLM.Props;LM;1;BBird

	enabled = MagnetoLE
	ApronMag_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;ApronMag
	ApronMag_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;ApronMag
	ApronMag_LM_Gi_Blue_l102a.Visible = enabled ' VLM.Props;LM;1;ApronMag
	ApronMag_LM_Gi_Blue_l102b.Visible = enabled ' VLM.Props;LM;1;ApronMag
	ApronMag_LM_Gi_White_l100a.Visible = enabled ' VLM.Props;LM;1;ApronMag
	ApronMag_LM_Gi_White_l100c.Visible = enabled ' VLM.Props;LM;1;ApronMag
	ApronMag_LM_Inserts_L29.Visible = enabled ' VLM.Props;LM;1;ApronMag
	ApronMag_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;ApronMag
	ApronMag_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;ApronMag
	ApronMag_LM_Flashers_f21.Visible = enabled ' VLM.Props;LM;1;ApronMag
	dim v1 : v1 = CInt(1 * (0.2 + 0.8 * LightLevel / 100.0))
	dim v2 : v2 = CInt(5 * (0.2 + 0.8 * LightLevel / 100.0))
	dim v3 : v3 = CInt(37 * (0.2 + 0.8 * LightLevel / 100.0))
	If MagnetoLE Then 'Change to Magneto LE Artwork
		PinCab_Backglass.image = "BackglassImage_Mag"
		PinCab_Backbox.image = "Pincab_Backbox_Mag"
		PinCab_Rails.image = "Color_Red"
		PinCab_Legs.image = "Color_Red"
		PinCab_Cabinet.image = "Pincab_Cabinet_Mag"
		PinCab_Buttons.material = "Colour_Red"
		y = v3 * 256 * 256 + v2 * 256 + v1 ' BGR color
	Else
		PinCab_Backglass.image = "BackglassImage"
		PinCab_Backbox.image = "Pincab_Backbox"
		PinCab_Rails.image = "Color_Blue"
		PinCab_Legs.image = "Color_Blue"
		PinCab_Cabinet.image = "Pincab_Cabinet"
		PinCab_Buttons.material = "Colour_Blue"
		y = v2 * 256 * 256 + v3 * 256 + v3 ' BGR color
	End If
	Dim ApronCol_BL: ApronCol_BL=Array(ApronCol_BM_Dark_Room, ApronCol_LM_Flashers_f31, ApronCol_LM_Lit_Room) ' VLM.Array;BL;ApronCol
	For Each bake in ApronCol_BL
		bake.Color = y
	Next
	
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	GetMaterial "CabRails", wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	base = CInt(64 + 191 * LightLevel / 100)
	base = base * 256 * 256 + base * 256 + base
	UpdateMaterial "CabRails", wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle

	If OutPostMod = 0 Then ' Easy
		enabled = True
		y = 0
		rout_hard_left.Collidable = False
		rout_med_left.Collidable = False
		rout_easy_left.Collidable = True
		rout_med_right.Collidable = False
		rout_easy_right.Collidable = True
	ElseIf OutPostMod = 1 Then ' Medium
		enabled = True
		y = 12
		rout_hard_left.Collidable = False
		rout_med_left.Collidable = True
		rout_easy_left.Collidable = False
		rout_med_right.Collidable = True
		rout_easy_right.Collidable = False
	ElseIf OutPostMod = 2 Then ' Hard
		enabled = False
		y = 24
		rout_hard_left.Collidable = True
		rout_med_left.Collidable = False
		rout_easy_left.Collidable = False
		rout_med_right.Collidable = False
		rout_easy_right.Collidable = False
	End If
	OutPostL_BM_Dark_Room.transy = y ' VLM.Props;BM;1;OutPostL
	OutPostL_LM_Lit_Room.transy = y ' VLM.Props;LM;1;OutPostL
	OutPostL_LM_Gi_White_l100e.transy = y ' VLM.Props;LM;1;OutPostL
	OutPostL_LM_Flashers_f17.transy = y ' VLM.Props;LM;1;OutPostL
	OutPostR_BM_Dark_Room.transy = y ' VLM.Props;BM;1;OutPostR
	OutPostR_LM_Lit_Room.transy = y ' VLM.Props;LM;1;OutPostR
	OutPostR_LM_Gi_Blue_l102d.transy = y ' VLM.Props;LM;1;OutPostR
	OutPostR_LM_Gi_White_l100g.transy = y ' VLM.Props;LM;1;OutPostR
	OutPostR_LM_Inserts_L69.transy = y ' VLM.Props;LM;1;OutPostR
	OutPostR_LM_Flashers_f21.transy = y ' VLM.Props;LM;1;OutPostR
	OutPostR_BM_Dark_Room.visible = enabled ' VLM.Props;BM;2;OutPostR
	OutPostR_LM_Lit_Room.visible = enabled ' VLM.Props;LM;2;OutPostR
	OutPostR_LM_Gi_Blue_l102d.visible = enabled ' VLM.Props;LM;2;OutPostR
	OutPostR_LM_Gi_White_l100g.visible = enabled ' VLM.Props;LM;2;OutPostR
	OutPostR_LM_Inserts_L69.visible = enabled ' VLM.Props;LM;2;OutPostR
	OutPostR_LM_Flashers_f21.visible = enabled ' VLM.Props;LM;2;OutPostR

	Lampz.state(150) = LightLevel

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

	' VR Mode
	Dim VRRoom
	If RenderingMode = 2 Then VRRoom = VRRoomChoice Else VRRoom = 0
	DIM VRThings
	If VRRoom > 0 Then
		ScoreText.visible = 0
		If VRRoom = 1 Then
			for each VRThings in VR_Cab:VRThings.visible = 1:Next
			for each VRThings in VR_Min:VRThings.visible = 1:Next
		End If
		If VRRoom = 2 Then
			for each VRThings in VRStuff:VRThings.visible = 0:Next
			PinCab_Backbox.visible = 1
			PinCab_Backglass.visible = 1
			DMD.visible = 1
		End If
	Else
			for each VRThings in VR_Cab:VRThings.visible = 0:Next
			for each VRThings in VR_Min:VRThings.visible = 0:Next
	End if

	'Cab mode (needs to come after VR Mode setting
	If CabinetMode and VRRoom=0 Then
		PinCab_Rails.visible = false
	Else
		PinCab_Rails.visible = true
	End If

	'*********************
	' Incandescent/LED lighting
	'*********************

	Dim l100_LM: l100_LM=Array(Playfield_LM_Gi_White_l100a, LEMK_LM_Gi_White_l100a, LFLogo_LM_Gi_White_l100a, RFLogo_LM_Gi_White_l100a, ApronMag_LM_Gi_White_l100a, Playfield_LM_Gi_White_l100i, Layer_1_LM_Gi_White_l100i, Layer_4_LM_Gi_White_l100i, BR2_LM_Gi_White_l100i, Playfield_LM_Gi_White_l100, Layer_1_LM_Gi_White_l100, Layer_3_LM_Gi_White_l100, Layer_4_LM_Gi_White_l100, BR1_LM_Gi_White_l100, RFLogo1_LM_Gi_White_l100, SpinnerT2_LM_Gi_White_l100, sw7p_LM_Gi_White_l100, Iceman_LM_Gi_White_l100, Wolverine_LM_Gi_White_l100, NCR_Top_LM_Gi_White_l100, Playfield_LM_Gi_White_l100j, Layer_1_LM_Gi_White_l100j, Layer_4_LM_Gi_White_l100j, sw54_LM_Gi_White_l100j, Playfield_LM_Gi_White_l100k, Layer_1_LM_Gi_White_l100k, Layer_2_LM_Gi_White_l100k, Layer_3_LM_Gi_White_l100k, Layer_5_LM_Gi_White_l100k, RampGate4_LM_Gi_White_l100k, SpinnerT2_LM_Gi_White_l100k, Sentinel_LM_Gi_White_l100k, Playfield_LM_Gi_White_l100b, Layer_4_LM_Gi_White_l100b, LEMK_LM_Gi_White_l100b, LFLogo_LM_Gi_White_l100b, LSling1_LM_Gi_White_l100b, LSling2_LM_Gi_White_l100b, RFLogo_LM_Gi_White_l100b, sw24_LM_Gi_White_l100b, sw25_LM_Gi_White_l100b, Playfield_LM_Gi_White_l100c, LFLogo_LM_Gi_White_l100c, REMK_LM_Gi_White_l100c, RFLogo_LM_Gi_White_l100c, ApronMag_LM_Gi_White_l100c, Playfield_LM_Gi_White_l100d, LFLogo_LM_Gi_White_l100d, REMK_LM_Gi_White_l100d, RFLogo_LM_Gi_White_l100d, RSling1_LM_Gi_White_l100d, RSling2_LM_Gi_White_l100d, sw28_LM_Gi_White_l100d, Playfield_LM_Gi_White_l100e, Layer_1_LM_Gi_White_l100e, Layer_4_LM_Gi_White_l100e, OutPostL_LM_Gi_White_l100e, sw1p_LM_Gi_White_l100e, Playfield_LM_Gi_White_l100f, Layer_1_LM_Gi_White_l100f, Layer_4_LM_Gi_White_l100f, BR3_LM_Gi_White_l100f, sw1p_LM_Gi_White_l100f, sw2p_LM_Gi_White_l100f, Playfield_LM_Gi_White_l100g, Layer_1_LM_Gi_White_l100g, Layer_3_LM_Gi_White_l100g, OutPostR_LM_Gi_White_l100g, sw8p_LM_Gi_White_l100g, Iceman_LM_Gi_White_l100g, Playfield_LM_Gi_White_l100h, Layer_1_LM_Gi_White_l100h, Layer_3_LM_Gi_White_l100h, RFLogo1_LM_Gi_White_l100h, sw7p_LM_Gi_White_l100h, sw8p_LM_Gi_White_l100h)
	Dim l101_LM: l101_LM=Array(Playfield_LM_GI_Red_l101a, Layer_4_LM_GI_Red_l101a, LEMK_LM_GI_Red_l101a, LFLogo_LM_GI_Red_l101a, LSling1_LM_GI_Red_l101a, LSling2_LM_GI_Red_l101a, sw24_LM_GI_Red_l101a, sw25_LM_GI_Red_l101a, Playfield_LM_GI_Red_l101g, Layer_3_LM_GI_Red_l101g, Sentinel_LM_GI_Red_l101g, Playfield_LM_GI_Red_l101b, REMK_LM_GI_Red_l101b, RFLogo_LM_GI_Red_l101b, RSling1_LM_GI_Red_l101b, RSling2_LM_GI_Red_l101b, sw28_LM_GI_Red_l101b, sw33_LM_GI_Red_l101b, Playfield_LM_GI_Red_l101c, Layer_1_LM_GI_Red_l101c, Layer_4_LM_GI_Red_l101c, BR3_LM_GI_Red_l101c, sw1p_LM_GI_Red_l101c, sw2p_LM_GI_Red_l101c, Wolverine_LM_GI_Red_l101c, Playfield_LM_GI_Red_l101d, Layer_1_LM_GI_Red_l101d, Layer_3_LM_GI_Red_l101d, RFLogo1_LM_GI_Red_l101d, sw7p_LM_GI_Red_l101d, sw8p_LM_GI_Red_l101d, Iceman_LM_GI_Red_l101d, Playfield_LM_GI_Red_l101e, Layer_1_LM_GI_Red_l101e, Layer_4_LM_GI_Red_l101e, Playfield_LM_GI_Red_l101, Layer_3_LM_GI_Red_l101, Layer_5_LM_GI_Red_l101, Playfield_LM_GI_Red_l101h, Layer_1_LM_GI_Red_l101h, Layer_4_LM_GI_Red_l101h, Target_004_LM_GI_Red_l101h, Playfield_LM_GI_Red_l101f, Layer_1_LM_GI_Red_l101f, Layer_2_LM_GI_Red_l101f, Layer_3_LM_GI_Red_l101f, Layer_4_LM_GI_Red_l101f, Layer_5_LM_GI_Red_l101f, Div_LM_GI_Red_l101f, SpinnerT2_LM_GI_Red_l101f, Target_001_LM_GI_Red_l101f, Sentinel_LM_GI_Red_l101f)
	Dim l102_LM: l102_LM=Array(Playfield_LM_Gi_Blue_l102a, Layer_4_LM_Gi_Blue_l102a, LEMK_LM_Gi_Blue_l102a, LFLogo_LM_Gi_Blue_l102a, LSling2_LM_Gi_Blue_l102a, sw24_LM_Gi_Blue_l102a, sw25_LM_Gi_Blue_l102a, ApronMag_LM_Gi_Blue_l102a, Playfield_LM_Gi_Blue_l102h, Layer_3_LM_Gi_Blue_l102h, Layer_4_LM_Gi_Blue_l102h, sw49_LM_Gi_Blue_l102h, Sentinel_LM_Gi_Blue_l102h, Playfield_LM_Gi_Blue_l102b, REMK_LM_Gi_Blue_l102b, RFLogo_LM_Gi_Blue_l102b, RSling1_LM_Gi_Blue_l102b, sw28_LM_Gi_Blue_l102b, sw29_LM_Gi_Blue_l102b, ApronMag_LM_Gi_Blue_l102b, Playfield_LM_Gi_Blue_l102c, Layer_1_LM_Gi_Blue_l102c, Layer_4_LM_Gi_Blue_l102c, BR3_LM_Gi_Blue_l102c, sw1p_LM_Gi_Blue_l102c, sw2p_LM_Gi_Blue_l102c, Playfield_LM_Gi_Blue_l102d, Layer_1_LM_Gi_Blue_l102d, Layer_3_LM_Gi_Blue_l102d, OutPostR_LM_Gi_Blue_l102d, sw8p_LM_Gi_Blue_l102d, Iceman_LM_Gi_Blue_l102d, Wolverine_LM_Gi_Blue_l102d, Playfield_LM_Gi_Blue_l102, Layer_1_LM_Gi_Blue_l102, Layer_3_LM_Gi_Blue_l102, BR1_LM_Gi_Blue_l102, BR3_LM_Gi_Blue_l102, RFLogo1_LM_Gi_Blue_l102, sw7p_LM_Gi_Blue_l102, Iceman_LM_Gi_Blue_l102, Wolverine_LM_Gi_Blue_l102, Playfield_LM_Gi_Blue_l102e, Layer_1_LM_Gi_Blue_l102e, Layer_4_LM_Gi_Blue_l102e, BR1_LM_Gi_Blue_l102e, BR2_LM_Gi_Blue_l102e, Playfield_LM_Gi_Blue_l102f, Layer_1_LM_Gi_Blue_l102f, Layer_4_LM_Gi_Blue_l102f, Wolverine_LM_Gi_Blue_l102f, Playfield_LM_Gi_Blue_l102g, Layer_3_LM_Gi_Blue_l102g, Layer_5_LM_Gi_Blue_l102g, Sentinel_LM_Gi_Blue_l102g)
	Dim l103_LM: l103_LM=Array(Playfield_LM_Gi_l103, Layer_1_LM_Gi_l103, Layer_3_LM_Gi_l103, Layer_4_LM_Gi_l103, Layer_5_LM_Gi_l103, Div_LM_Gi_l103, Iceman_LM_Gi_l103, Sentinel_LM_Gi_l103, Playfield_LM_Gi_l103a, Layer_1_LM_Gi_l103a, Layer_4_LM_Gi_l103a, Iceman_LM_Gi_l103a, Wolverine_LM_Gi_l103a, BBird_LM_Gi_l103a, NCR_Top_LM_Gi_l103a, Playfield_LM_Gi_l103b, REMK_LM_Gi_l103b, RFLogo1_LM_Gi_l103b, RSling1_LM_Gi_l103b, RSling2_LM_Gi_l103b, Iceman_LM_Gi_l103b, Wolverine_LM_Gi_l103b, NCL_Top_LM_Gi_l103b)

	Dim c, d, e
	If IncandescentGI Then
		c = RGB(255,169, 87) ' 2700K from https://andi-siess.de/rgb-to-color-temperature/
		d = RGB( 10, 26, 87)
		e = RGB(255,  9,  2)
	Else
		c = RGB(255,255,255)
		d = RGB( 10, 39,255)
		e = RGB(255, 13,  6)
	End If
	For Each x in GI_Base  : x.color = c : Next
	For Each x in GI_Red   : x.color = e : Next
	For Each x in GI_Blue  : x.color = d : Next
	For Each x in GI_White : x.color = c : Next
	For Each bake in l100_LM : bake.Color = c : Next ' White
	For Each bake in l101_LM : bake.Color = e : Next ' Red
	For Each bake in l102_LM : bake.Color = d : Next ' Blue
	For Each bake in l103_LM : bake.Color = c : Next ' White
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
	VLM_Warmup_Nestmap_15.Visible = False
	VLM_Warmup_Nestmap_16.Visible = False
	VLM_Warmup_Nestmap_17.Visible = False
	VLM_Warmup_Nestmap_18.Visible = False
	VLM_Warmup_Nestmap_19.Visible = False
	VLM_Warmup_Nestmap_20.Visible = False
	VLM_Warmup_Nestmap_21.Visible = False
End Sub

'****************************************************************
'	Changelog
'****************************************************************
'v2.0.1 - Sixtoe / DJRobX - Initial base version
'v2.1.0 - Niwak - Blender Toolkit Initial Import
'v2.1.1 - Sixtoe - Cleaned VPX
'v2.1.2 - Sixtoe - Redid lots of physical objects, added all (?) nfozzy physics, added various shadows, added physical trough, redid physical kickers, fixed ball traps, added playfield physics mesh, added fleep sounds, probably other things I've forgotten,
'v2.1.3 - apophis - Rebuilt physical version of bsLHole and bsL. Made dPosts, dSleeves, Rollovers collections fire hit events. Created Apron and Walls collections, Made flipper triggers 150 units high. Updated some flipper physics params. Cleaned up spotlight shadows implementation. Removed unneeded DynamicLamps class.
'v2.1.4 - Sixtoe - Fixed kickers (grmbl), new prim playfield, new vuk guide prim, fixed iceman wall start layout as ball was catching there, fixed some broken walls, added missing aounds
'v2.1.5 - apophis - Converted VUKs to use switches and kickball sub rather than kickers. Fixed the ball shadow conflict with flipper shadows. A few minor physics parameter adjustments. 
'v2.1.6 - Niwak - animated switch wires, fix spinner animation, hide visible physics, fix orbit diverter
'v2.1.7 - Sixtoe - Rebuilt shooter lane, messed around with the blackbird physical hole and it seems to be working with 2 balls as well now (hopefully that's it now forever and I never have to look at it again, ever.).disabled a few more duplicate prims (including bumpers), hooked up cabinetmode and vrroom modes.
'v2.1.8 - Niwak - Fix iceman ramp being stuck, fix slingshot anim, fade lightmaps on iceman ramp depending on position
'v2.1.9 - Sixtoe - Hid things which should have been invisible.
'v2.2.0 - Sixtoe - Upped playfield friction to 0.24, calibrated plunger, changed ball, added collision sounds to wolverine and better sounds to nightcrawler, turned up spinner fractionally, added triggers and enabled ramprolling sounds.
'v2.2.1 - Niwak - Add option UI, add blackbird mod, add staged flippers, add dual lighting, fix base GI, adjust ball brightness
'v2.2.2 - Sixtoe - Tweaked magneto turntable so it tries to kill you more effectively now, tuned slings down a bit, fixed the iceman ramp motor sound I broke,
'v2.2.3 - Niwak - Full rebake, more backpanel GI light, new lit room setting, better blackbird mod, complete magneto mod, fix flipper bat size, fix upper post bake, add outpost difficulty setting, adjust rails light level
'v2.2.4 - Sixtoe - Added physical C scoop for blackbird, various table fixes, table and script tidy-ups.
'v2.2.5 - Niwak - Fix diverter position, fix wolverine entering Scoop
'v2.2.6 - Sixtoe - Resized pop bumpers and rebuilt pop bump area slightly, rebuilt left ramp to avoid clipping through beast wall, shrunk blackbird scoop metal, tweaked magneto magnet.
'v2.2.7 - Sixtoe - Fixed scoop exit so ball doesnt his iceman ramp when it's sideways, apron made higher to catch flyballs, turned down nudge, droped underplayfield layer, cleaned up unused images.
'v2.2.8 - Sixtoe - Fixed Playfield Lighting, Turned down flasher reflections, adjusted xavier kickout, turned a couple of things invisible, boosted apron lamps, fixed some minor gi issues, minor tweaks.
'v2.2.9 - Sixtoe - Renamed TriggerRF, Upped nightcrawler speed from 1 to 1.5, adjusted right niqhtcrawler hit threshold.
'v2.3.0 - Niwak - Rebake with higher resolution, baked spinner and flipper bats, updated gate animations
'v2.3.1 - Niwak - Fix lock diverter, add baked plunger, dynamic ball color & brightness
'v2.3.2 - Niwak - Adjust ball coloring, disable bloom, AO and screenspace reflections
'v2.3.3 - apophis - Iceman ramp now works while a ball is rolling on it! Made Lampz use only one timer with -1 interval.
'v2.3.4 - Niwak - Rework of lots of parts using parts from the pinball core part library, full 4K rebake with latest toolkit
'v2.3.5 - Niwak - Reworked plastic, magnets, playfield holes, missing rail guide fixtures
'v2.3.6 - Niwak - Add details to account for ramps being wrong. Prepare for 10.8 (support for refraction)
'v2.3.7 - Niwak - Fix wrong bakes, add support for VR autodetection, move VR options to in-game menu, use new toolkit arrays, fix sling animation, fix Wolverine not registering too strong hit
'v2.3.8 - apophis - Fixed slope of iceman ramp collidable floor.
'v2.3.9 - iaakki - Magnet helper to prevent 4th ball escaping the hold
'v2.4.0 - Wylte - Shadow update, commented out speeddownlim enabled/disabled references, lowered rubber hit volumes, metals hit sounds added to scoop
'v2.4.1 - iaakki - fixed magnet helper
'v2.4.2 - apophis - Reduced flipper length from 120 to 114.7 (and associated primitives). Reduced flipper strength from 3200 to 3100.
'v2.4.3 - apophis - Improved robustness of left scoop walls/floor. Added Narnia catcher. Added Magneto lock pin solenoid sounds. Removed ball collision sounds under Magneto. Removed targetbouncer from both slings bottom post. Made Wolverine magnet grabcenter=false (more realistic). Rebuilt Wolverine wobble.
'v2.4.4 - Niwak - Adjusted baked flipper bats to match physics one
'RC1	- apophis - Reduced digital nudge strength. Increased tilt sensitivity. 
'RC2	- Sixtoe - Added visible apron wall for VR, manually fixed magneto texture (VLM.Nestmap0), adjusted sw53 and removed group properties, added missing one way gate behind magneto (to stop balls being knocked out top of lock), added missing rubbers by top diverter, 
'RC3	- apophis - Added code to kick ball out of the way if on top of night crawlers
'RC4	- apophis - Added desaturation LUTs to option menu. Fixed cab mode option menu setting (needed to be applied after VR room setting). Added AstroNasty's artwork to desktop background.
'RC5	- Niwak - Added light reflections from inserts on ball.
'RC6	- apophis - Fixed conflict between cabinetmode and vrroom.
'RC7	- Niwak - Update render: align right sling spot, new magnets, dust on ramps, less saturated red/blue GI
'RC8	- Niwak - Full render rework: new plastics, fixed playfield, new ramps, improved magneto, moved Sentinel to a mod
'RC9	- Sixtoe - Redid physical ramps and upper layer and realigned all triggers, tweaked ramp entries, changed the magneto opto's and blackbird rollover to 23 & 24 radius, moved right sling spotlight shadows to match new position, fixed broken prioritys with spotlight shadows vs. rtx shadows, fixed playfield mesh alignment issue and blackbird hole feed, added bevel to blackbird hole, added some dampening to the rubber feed for blackbird, added ball velocity dampning for inlanes (thanks apophis), rebuilt primitive collidable iceman ramps
'RC10   - Niwak - Fix missing flashers and little bake errors, fix under playfield visibility for VPX<10.8, enable 10.8 ball shadows
'RC11   - Niwak - Add incandescent bulb option, deactivate 10.8 stuff for initial release
'RC12	- Sixtoe - Upped ramp friction to 0.2 to match Iron Man.
'v1.0 Release
