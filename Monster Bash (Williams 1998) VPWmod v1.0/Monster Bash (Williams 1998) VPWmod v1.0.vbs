'Monster Bash - IPDB No. 4441
'© Williams 1998
'
'VPW Monsters
'============
'Project Lead - Skitso
'Lighting - Skitso, iaakki
'New Tesla Coil Modulated Flashers - iaakki, Leojremimroc
'Various Fixes / Tweaks - Rothbauerw, fluffhead35, iaakki, Sixtoe, apophis, Leojremimroc
'VR Backglass - Leojremimroc
'New Wire Ramps - Tomate
'
'Based on earlier versions;
'Skitso mod by team rothborski - Skitso, rothbauerw, bord & thanks to VPW team.
'thanks to tom tower & ninuzzu,unclewilly and randr for previous versions
'thanks to flupper and zany for the domes and bumpers

'thanks to VPDev Team for the freaking amazing VPX
'*Full changelog at end of script

Option Explicit
Randomize

'******************************************************
' 						OPTIONS
'******************************************************

'///////////////////////-----General Sound Options-----///////////////////////
'//  VolumeDial:
'//  VolumeDial is the actual global volume multiplier for the mechanical sounds.
'//  Values smaller than 1 will decrease mechanical sounds volume.
'//  Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'/////////////////////-----BallRoll Sound Amplification -----/////////////////////
Const BallRollAmpFactor = 0 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)

'-----RampRoll Sound Amplification -----/////////////////////
Const RampRollAmpFactor = 0 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)

'///////////////////////-----Outlane Difficulty-----///////////////////////
Const OutLaneDifficulty = 1	' 2 - Hard (default), 1 - Medium, 0 - Easy

'////////////////////////////-----VR Room-----/////////////////////////////
Const VRRoom = 0 				' 0 - VR Room off, 1 - 360 Room, 2 - Minimal Room, 3 - Ultra Minimal
Const VRFlashingBackglass = 1	' 0 - Flashing backglass Off, 1 - Flashing backglass On	

'////////////////////////////-----Cabinet mode-----/////////////////////////////
'Will hide the rails and scale the side panels higher
Const CabinetMode = 0

'////////////////////////-----Lightning Flasher-----///////////////////////
Const Lightning = 0 ' 0 - Off, 1 - On

'////////////////////////-----Phsyics Mods----------///////////////////////
Const Rubberizer = 1			'0 - disable, 1 - rothbauerw version, 2 - iaakki version
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's

'************************************************************************
'						 INIT TABLE
'************************************************************************

Const BallSize = 50
Const BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode

Const UseVPMModSol = 1

If DesktopMode then ScoreText.visible = 1 else ScoreText.visible = 0

' Rom Name
Const cGameName = "mb_106b"

LoadVPM "02000000", "WPC.VBS", 3.50

'********************
'Standard definitions
'********************

' Standard Options
Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 1
Const HandleMech = 0 
Const cSingleLFlip = 0
Const cSingleRFlip = 0

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = ""

' using table width and height in script slows down the performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

Dim xx, MBBall1, MBBall2, MBBall3, MBBall4

Sub Table1_Init
	vpmInit Me 
	With Controller      
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Monster Bash Williams 1998"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 4
		.Hidden = DesktopMode
		.dip(0)=&h00  'Set DIP to USA (this fixes the Phantom Flip lights)
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
		.Switch(22) = 1 'close coin door
		.Switch(24) = 0 'always closed
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

 	'************  Trough	**************************
	Set MBBall4 = sw35.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBBall3 = sw34.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBBall2 = sw33.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBBall1 = sw32.CreateSizedballWithMass(Ballsize/2,Ballmass)
	
	Controller.Switch(35) = 1
	Controller.Switch(34) = 1
	Controller.Switch(33) = 1
	Controller.Switch(32) = 1

	FrankInit
	TargetsInit
	CreatureInit
	UpdateGI 0,0:UpdateGI 1,0:UpdateGI 2,0
	lock.IsDropped = 1							'ramp lock post

	If OutLaneDifficulty = 2 Then
		LeftOutlanePeg.y = 1405
		RightOutlanePeg.y = 1405

		LOM1.visible = False	
		LOM1.collidable = False
		LOM2.visible = False	
		LOM2.collidable = False

		LOE1.visible = False	
		LOE1.collidable = False
		LOE2.visible = False	
		LOE2.collidable = False

		ROM1.visible = False	
		ROM1.collidable = False
		ROM2.visible = False	
		ROM2.collidable = False

		ROE1.visible = False	
		ROE1.collidable = False
		ROE2.visible = False	
		ROE2.collidable = False
	ElseIf OutLaneDifficulty = 1 Then
		LeftOutlanePeg.y = 1422
		RightOutlanePeg.y = 1422

		LOH1.visible = False	
		LOH1.collidable = False
		LOH2.visible = False	
		LOH2.collidable = False

		LOE1.visible = False	
		LOE1.collidable = False
		LOE2.visible = False	
		LOE2.collidable = False

		ROH1.visible = False	
		ROH1.collidable = False
		ROH2.visible = False	
		ROH2.collidable = False

		ROE1.visible = False	
		ROE1.collidable = False
		ROE2.visible = False	
		ROE2.collidable = False
	Else
		LeftOutlanePeg.y = 1439
		RightOutlanePeg.y = 1439

		LOH1.visible = False	
		LOH1.collidable = False
		LOH2.visible = False	
		LOH2.collidable = False

		LOM1.visible = False	
		LOM1.collidable = False
		LOM2.visible = False	
		LOM2.collidable = False

		ROH1.visible = False	
		ROH1.collidable = False
		ROH2.visible = False	
		ROH2.collidable = False

		ROM1.visible = False	
		ROM1.collidable = False
		ROM2.visible = False	
		ROM2.collidable = False
	End If
End Sub

'************************************************************************
'							KEYS
'************************************************************************

Sub Table1_KeyDown(ByVal Keycode)
	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 6:SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

		End Select
	End If

	If keycode = PlungerKey Then Controller.Switch(11) = 1: soundStartButton()

	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress

'	If keycode = LeftMagnaSave Then flashsol19 255
'	If keycode = RightMagnaSave Then flashsol19 55

	if keycode=StartGameKey then soundStartButton()
	
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If keycode = PlungerKey Then Controller.Switch(11) = 0

	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused:Controller.Pause = True:End Sub
Sub Table1_unPaused:Controller.Pause = False:End Sub
Sub Table1_exit():Controller.Pause = False:Controller.Stop:End Sub

'************************************************************************
'						 SOLENOIDS
'************************************************************************

SolCallback(1) = "AutoPlunger"													'AutoPlunger
SolCallback(2) = "SolBride"														'Bride Post
SolCallback(3) = "SolMummy"														'Mummy Coffin
SolCallback(5) = "vpmSolGate LGate,false,"										'Left Gate
SolCallback(6) = "vpmSolGate Rgate,false,"										'Right Gate
SolCallback(7) = "solKnocker"													'Knocker
SolCallback(8) = "SolLockPost"													'Ramp Lock Post
SolCallback(9) = "SolBallRelease"												'Trough Eject
'SolCallback(10) = "vpmSolSound SoundFX(""LeftSlingshot"",DOFContactors),"		'Left Sling		
'SolCallback(11) = "vpmSolSound SoundFX(""RightSlingshot"",DOFContactors),"		'Right Sling	
'SolCallback(12) = "vpmSolSound SoundFX(""LeftJet"",DOFContactors),"				'Left Bumper
'SolCallback(13) = "vpmSolSound SoundFX(""RightJet"",DOFContactors),"			'Right Bumper	
'SolCallback(14) = "vpmSolSound SoundFX(""BottomJet"",DOFContactors),"			'Bottom Bumper
SolCallback(15) = "SolSaucer"													'Left Eject
SolCallback(16) = "SolRightScoop"												'Right Popper
SolModCallback(17) = "SetModLamp 117,"											'WolfMan Flashers
SolModCallback(18) = "SetModLamp 118,"											'Bride Flashers
SolModCallback(19) = "Flashsol19" 												'Frank Flashers
SolModCallback(20) = "SetModLamp 120,"											'Dracula Flashers
SolModCallback(21) = "SolCreature"												'Creature Flashers
SolModCallback(22) = "SetModLamp 122,"											'Mummy Flashers
SolCallback(23) = "FlashSol23"  '"SetModLamp 123,"								'Right Popper Flashers
SolModCallback(24) = "SetModLamp 124,"											'Frank Arrow Flashers
SolModCallback(25) = "SetModLamp 125,"											'Monsters of rock Flashers
SolCallback(26) = "SetLamp 126,"												'WolfMan Loop Flashers
SolCallback(27) = "SolFrank"													'Frank Motor
SolCallback(28) = "SolBank"														'Bank Motor

SolCallback(41) = "SolDrac"														'Dracula Motor

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

'******************************************************
'					KNOCKER
'******************************************************
Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

'******************************************
'				FLIPPERS
'******************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
	
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
		RF.Fire 'rightflipper.rotatetoend

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

'************************************************************************
'						 AUTOPLUNGER 
'************************************************************************

Sub AutoPlunger(Enabled)
	If Enabled Then
		Plunger.Fire
		PlaySoundAt SoundFX("AutoPlunger",DOFContactors), Plunger
	End If
End Sub

'******************************************************
'						TROUGH 
'******************************************************

Sub sw34_Hit():Controller.Switch(34) = 1:UpdateTrough:End Sub
Sub sw34_UnHit():Controller.Switch(34) = 0:UpdateTrough:End Sub
Sub sw33_Hit():Controller.Switch(33) = 1:UpdateTrough:End Sub
Sub sw33_UnHit():Controller.Switch(33) = 0:UpdateTrough:End Sub
Sub sw32_Hit():Controller.Switch(32) = 1:UpdateTrough:End Sub
Sub sw32_UnHit():Controller.Switch(32) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw32.BallCntOver = 0 Then sw33.kick 60, 9
	If sw33.BallCntOver = 0 Then sw34.kick 60, 9
	If sw34.BallCntOver = 0 Then sw35.kick 60, 20
	Me.Enabled = 0
End Sub

'******************************************************
'					DRAIN & RELEASE
'******************************************************

Sub sw35_Hit() 'Drain
	UpdateTrough
	Controller.Switch(35) = 1
	RandomSoundDrain sw35	
End Sub

Sub sw35_UnHit()  'Drain
	Controller.Switch(35) = 0
End Sub

Sub SolBallRelease(enabled)
	If enabled Then 
		If sw32.BallCntOver = 0 Then
			SoundSaucerKick 0, sw32
		Else
			RandomSoundBallRelease sw32
			vpmTimer.PulseSw 31
		End If
		sw32.kick 60, 9		
	End If
End Sub

'************************************************************************
'						 LEFT EJECT
'************************************************************************

Sub sw28_hit
	SoundSaucerLock
	Controller.switch(28) = True
End Sub

Sub sw28_unhit
	Controller.switch(28) = False
End Sub

Sub SolSaucer(Enabled)
	If Enabled Then
		If controller.Switch(28) Then
			SoundSaucerKick 1, sw28
		Else
			SoundSaucerKick 0, sw28
		End If
		sw28.kick 202 + rnd*6, 17 + rnd*2, radians(45)
	End If
End Sub

'************************************************************************
'						 RIGHT SCOOP
'************************************************************************
Dim sbrake,sshake
Sub SolRightScoop(enabled)
	If enabled then
		Playsoundat SoundFx("fx_mine_popper",DOFContactors), sw36
		sw36.kickz 196+(rnd*2-1), 24+(rnd*2-1),0,140 
		controller.Switch(36) = 0
	End If
End Sub

Sub sw36_hit:Controller.Switch(36)=1:Playsoundat "fx_kicker_catch", sw36:End Sub

Sub ScoopShake_timer
	if sbrake>0 then			
		sshake=sshake+30
		sbrake=sbrake-0.1
		ScoopP.transy =((dsin(sshake)))*sbrake
	else
		me.enabled=0:ScoopP.transy=0:sbrake=0:sshake=0
	End if
End Sub

'************************************************************************
'						SLINGSHOTS
'************************************************************************

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
	RandomSoundSlingshotLeft sling1
	vpmTimer.PulseSw 51
	LSling.Visible = 0
	LSling1.Visible = 1
	sling1.TransZ = -20
	LStep = 0
	Me.TimerInterval = 20
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling1.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RandomSoundSlingshotRight sling2
	vpmTimer.PulseSw 52
	RSling.Visible = 0
	RSling1.Visible = 1
	sling2.TransZ = -20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling2.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'************************************************************************
'							BUMPERS
'************************************************************************

Sub Bumper1_Hit:vpmTimer.PulseSw 53:RandomSoundBumperTop bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 54:RandomSoundBumperMiddle bumper2:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 55:RandomSoundBumperBottom bumper3:End Sub

'************************************************************************
'						BRIDE ANIMATION
'************************************************************************

Sub SolBride(Enabled)
	If enabled then
		PlaySoundat SoundFX("IdolReleaseOn",DOFContactors), brideh
		BrideH.Z = BrideH.Z + 20
	Else
		PlaySoundat SoundFX("IdolReleaseOff",DOFContactors), brideh
		BrideH.Z = BrideH.Z - 20
	End if
End Sub

'************************************************************************
'   					MUMMY ANIMATION
'************************************************************************
Dim cofdir, cbrake, cshake

Sub SolMummy(Enabled)
 	if enabled then
		PlaySoundat SoundFX("IdolReleaseOn",DOFContactors), mumcoffin
		cofdir=1:mummycoffin.enabled=1
		CoffinShake.Enabled=0:Mumcoffin.transy=0:Mumcoffin3.transy=0:cbrake=0:cshake=0
	else
		PlaySoundat SoundFX("IdolReleaseOff",DOFContactors), mumcoffin
		cofdir=-1:mummycoffin.enabled=1
		CoffinShake.Enabled=0:Mumcoffin.transy=0:Mumcoffin3.transy=0:cbrake=0:cshake=0
	End If
End Sub

  Sub MummyCoffin_Timer()
	Mumcoffin.RotY = Mumcoffin.RotY + cofdir*10
	mumcoffin2.roty=Mumcoffin.RotY:mumcoffin3.roty=Mumcoffin.RotY
	If Mumcoffin.RotY >75 and cofdir=1 then Me.Enabled=0:CoffinShake.Enabled=1:cbrake=5
	If Mumcoffin.RotY <0 and cofdir=-1 then Me.Enabled=0
End Sub

sub CoffinShake_timer()
	if cbrake>0 then			
		cshake=cshake+30
		cbrake=cbrake-0.1
		Mumcoffin.transy =((dsin(cshake)))*cbrake
		mumcoffin3.transy=Mumcoffin.transy
	else
		me.enabled=0:Mumcoffin.transy=0:Mumcoffin3.transy=0:cbrake=0:cshake=0
	End if
End Sub


'************************************************************************
'					RAMP LOCK POST
'************************************************************************

Sub SolLockPost(Enabled)
	if enabled then
		lock.isdropped=0:PlaySoundat SoundFX("TopPostUp",DOFContactors), LockP:LockP.Z = 50 : RRPost.Enabled = 1
	else
		lock.isdropped=1:PlaySoundat SoundFX("TopPostDown",DOFContactors), LockP:LockP.Z = -50
	End if
End Sub

'************************************************************************
'					CREATURE ANIMATION
'************************************************************************
Dim crot,cBall,ShakeEnabled:ShakeEnabled = 0:crot=0

Sub CreatureInit
	Set cBall = ckicker.createball
	ckicker.Kick 0, 0
End Sub

Sub SolCreature(value)
	SetModLamp 121,value
	If value>0 then
		ShakeEnabled = 1
		CreatureCover.visible = 0
		Creatureshake.enabled = 1
	Else
		CreatureCover.visible = 1
		CreatureWait.Interval=2000	'wait 2 seconds then stop the creature shake
		CreatureWait.enabled = 1
	End if
End Sub

Sub Creatureshake_timer
     crot=crot+0.35
     creature.rotz=creature.rotz-(Sin(crot))*0.15
end Sub

Sub CreatureWait_timer	' stop the creature shake
	Me.Enabled=0: Creatureshake.Enabled=0:crot=0:creature.RotZ=0:ShakeEnabled = 0
End Sub

'************************************************************************
'						FRANK TARGETS BANK
'************************************************************************

Sub sw85_Hit:STHit 85: End Sub
Sub sw86_Hit:STHit 86: End Sub

Sub TargetsInit
	'starts the bank in down position
	frankytargets.z=-11
	sw85b.Isdropped = 0
	sw86b.Isdropped = 0
	sw85.collidable = True
	sw86.collidable = True
	Controller.Switch(81) = 1			
	Controller.Switch(82) = 0
End Sub

Sub SolBank(enabled)
	If enabled then
		PlaySound SoundFX("fx_mine_motor", DOFGear), -1, 0.5, -0.1, 0, 0, 1, 0
		BankMove.enabled=1
	Else
		StopSound "fx_mine_motor"
		BankMove.enabled=0
	End If
End Sub

dim tdir,ftargvel
ftargvel=0.2:tdir=-1

Sub BankMove_timer()
    if tdir=-1 and frankytargets.z>-75.5 then 
          frankytargets.z=frankytargets.z-ftargvel
          if frankytargets.z<-75 then 
		Controller.Switch(82) = 1
		Controller.Switch(81) = 0
		sw85b.isdropped=1
		sw86b.isdropped=1
		sw85.collidable = False
		sw86.collidable = False
		tdir=1
		Me.enabled=0
	end If
    End if
    if tdir=1 and frankytargets.z<-10 then
          frankytargets.z=frankytargets.z+ftargvel
          if frankytargets.z>-11 then 
		Controller.Switch(81) = 1
		Controller.Switch(82) = 0
		sw85b.isdropped=0
		sw86b.isdropped=0
		sw85.collidable = True
		sw86.collidable = True
		tdir=-1
		Me.enabled=0
	End If
    End if
End Sub

dim tbrake,tshake

sub bankshake_timer()
	if tbrake>0 then			
		tshake=tshake+30
		tbrake=tbrake-0.08
		frankytargets.transy =((dsin(tshake)))*tbrake
	else
		tbrake=0:tshake=0:me.enabled=0
	End if
End Sub

'************************************************************************
'				FRANKY ANIMATION
'************************************************************************
'83 table down
'84 table up
'87 frank hit

Dim FrankDir,velfranky,compyfrank,compzfrank,fmove,canplay:canplay=0:velfranky=0.3

Sub frankinit
	franktable.rotx=0
	Controller.Switch(84) = 1
	Controller.Switch(87) = 1
	fhitwall.isdropped=1
End Sub

Sub SolFrank(enabled)
	If Enabled Then
		PlaySound SoundFX("Bridge_Move", DOFGear), -1, 0.1, 0, 0, 0, 1, 0
		if franky.rotx<-40 then FrankDir=-1:FrankMove.Enabled = 1:canplay=1
		if franky.rotx>-40 then FrankDir=1:FrankMove.Enabled = 1:canplay=1
	Else
		FrankMove.Enabled = 0::canplay=0
		StopSound "Bridge_Move":Playsound SoundFX("Bridge_Stop", 0), 0, 0.1, 0, 0, 0, 1, 0
	End If
End Sub

'*******Franky body/arms/bracket/table rotation****************************************

Sub FrankMove_timer
    fmove=1
	Select Case FrankDir
		Case 1		'move frank down
		'---------------------------------------------------------------------------------------
			if franky.rotx<-85 then Controller.Switch(83) = 1:Controller.Switch(84) = 0:fhitwall.isdropped=0:fhitwallhelp.isdropped=0 else fhitwall.isdropped=1:fhitwallhelp.isdropped=1
			if franky.rotx>-80 then Controller.Switch(87) = 1 else Controller.Switch(87) = 0
		'---------------------------------------------------------------------------------------
			if franktable.rotx>-75 then 
			   franktable.rotx=franktable.rotx-velfranky
			If franksh.size_Y> 0.3 then franksh.size_y = franksh.size_y - velfranky/120:franksh.transY= franksh.transY + velfranky/120
		'------------------------------------------------------
			   franky.rotx=franky.rotx-velfranky
			   franky.z=114*(dcos(franktable.rotx+270))+138
			   franky.y=114*(dsin(franktable.rotx-270))+622
			   franky.transx=(dsin(franky.rotx))*5
		'------------------------------------------------------
			   frankybraket.x=351 + (frankybraket.y/12)
			   frankybraket.rotx=franktable.rotx/5
			   frankybraket.z=122*(dcos(franktable.rotx+90))+140
			   frankybraket.y=122*(dsin(franktable.rotx+270))+623

		'------------------------------------------------------
				compyfrank=((-156)*(dSIN(franky.rotx+90))+FRANKY.Y)
				arms.y=(-39*(dSin(franky.rotx))+compyfrank)
				compzfrank =((-156)*(dsin(franky.rotx+0))+FRANKY.z)
				arms.z=(39*(dcos(franky.rotx))+ compzfrank)
				arms.x=349+(dsin(franky.rotx+90))*-12
		'------------------------------------------------------
			End if
			if franktable.rotx<-70 then
				if canplay=1 then PlaySound SoundFX("frankhit",DOFShaker):canplay=0
				if franky.rotx>-90 then
					franky.rotx=franky.rotx-2
					franky.transx=(dsin(franky.rotx))*5
					compyfrank=((-156)*(dSIN(franky.rotx+90))+FRANKY.Y)
					arms.y=(-39*(dSin(franky.rotx))+compyfrank)
					compzfrank =((-156)*(dsin(franky.rotx+0))+FRANKY.z)
					arms.z=(39*(dcos(franky.rotx))+ compzfrank)
					arms.x=349+(dsin(franky.rotx+90))*-12
				else
					fshake=0:fbrake=1 : mainshake.enabled=1:fmove=0:Me.enabled=0
			   End if 
			End if

		Case -1		'move frank up
		'---------------------------------------------------------------------------------------
			if franky.rotx>-80 then Controller.Switch(87) = 1 else Controller.Switch(87) = 0
			if franky.rotx>-5 then Controller.Switch(83) = 0:Controller.Switch(84) = 1:fhitwall.isdropped=1:fhitwallhelp.isdropped=1
		'---------------------------------------------------------------------------------------
			if franktable.rotx<0 then 
				franktable.rotx=franktable.rotx+velfranky 
				If franksh.size_Y< 1 then franksh.size_y = franksh.size_y + velfranky/120:franksh.transY= franksh.transY - velfranky/120
		'------------------------------------------------------
				franky.rotx=franky.rotx+velfranky
				franky.z=114*(dcos(franktable.rotx+270))+138
				franky.y=114*(dsin(franktable.rotx-270))+622
				franky.transx=(dsin(franky.rotx))*5
		'------------------------------------------------------
				frankybraket.x=351 + (frankybraket.y/12)
				frankybraket.rotx=franktable.rotx/5
				frankybraket.z=122*(dcos(franktable.rotx+90))+140
				frankybraket.y=122*(dsin(franktable.rotx+270))+623
		'------------------------------------------------------
				compyfrank=((-156)*(dSIN(franky.rotx+90))+FRANKY.Y)
				arms.y=(-39*(dSin(franky.rotx))+compyfrank)
				compzfrank =((-156)*(dsin(franky.rotx+0))+FRANKY.z)
				arms.z=(39*(dcos(franky.rotx))+ compzfrank)
				arms.x=349+(dsin(franky.rotx+90))*-12
		'------------------------------------------------------
				if franktable.rotx>-55 then
					if franky.rotx< franktable.rotx then
						franky.rotx=franky.rotx+1
						compyfrank=((-156)*(dSIN(franky.rotx+90))+FRANKY.Y)
						arms.y=(-39*(dSin(franky.rotx))+compyfrank)
						compzfrank =((-156)*(dsin(franky.rotx+0))+FRANKY.z)
						arms.z=(39*(dcos(franky.rotx))+ compzfrank)
						arms.x=349+(dsin(franky.rotx+90))*-12
					End if 
				End if
			else
				fmove=0:Me.enabled=0
			End if
	End Select
End Sub

'*******Franky Shake****************************************   
dim fhit, fhitspeed

Sub fhitwall_Hit
	fhitspeed=Int(SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely))
	frankyshake.enabled=1:fhit=1	
End Sub

Sub frankyshake_timer
	if fmove=0 then 
		if franky.rotx>-76 then Controller.Switch(87) = 1 else Controller.Switch(87) = 0
		if franky.rotx<-75 and fhit=1 then franky.rotx=franky.rotx+(fhitspeed/5) else fhit=0
		if franky.rotx>(-90+(fhitspeed*3))then fhit=0
		if fhit=0 and franky.rotx>-90  then franky.rotx=franky.rotx-0.5
		if franky.rotx<-89 then fshake=0 : fbrake=1 : mainshake.enabled=1:frankyshake.enabled=0
		arms.rotx=(franky.rotx+90)*2
		compyfrank=((-156)*(dSIN(franky.rotx+90))+FRANKY.Y)
		arms.y=(-39*(dSin(franky.rotx))+compyfrank)
		compzfrank =((-156)*(dsin(franky.rotx+0))+FRANKY.z)
		arms.z=(39*(dcos(franky.rotx))+ compzfrank)
		arms.x=349+(dsin(franky.rotx+90))*-12
	End if
End Sub

dim fbrake,fshake

Sub mainshake_timer()
	if fbrake>0 then
		fshake=fshake+30
		fbrake=fbrake-0.04
		franky.rotx =franky.rotx + ((dsin(fshake)))*fbrake
	else
		fbrake=0:fshake=0:mainshake.enabled=0
	End if

	arms.rotx=(franky.rotx+90)*4
	compyfrank=((-156)*(dSIN(franky.rotx+90))+FRANKY.Y)
	arms.y=(-39*(dSin(franky.rotx))+compyfrank)
	compzfrank =((-156)*(dsin(franky.rotx+0))+FRANKY.z)
	arms.z=(39*(dcos(franky.rotx))+ compzfrank)
	arms.x=349+(dsin(franky.rotx+90))*-12
End Sub 

'************************************************************************
'					DRAC ANIMATION
'************************************************************************

Dim MechPos, dOldPos, dNewPos, dDir, initDrac
initDrac = 0

'*******Drac Hit****************************************
Sub DracTargets_Hit(idx)
    vpmTimer.PulseSw 25:DBRAKE=3:DracShake.enabled=1
End Sub

'*******Drac Move****************************************
Sub SolDrac(enabled)
	If Enabled Then
		PlaySound SoundFX("IdolMotor",DOFGear),-1
	Else
		StopSound "IdolMotor"
	End If
End Sub

Sub DracUpdate
	dOldPos=MechPos

	MechPos=controller.getmech(2)
	If MechPos < 5 then MechPos = 5
	If MechPos > 85 then MechPos = 85

	dNewPos=MechPos

	if dOldPos>dNewPos then dDir=1:DOF 102, DOFPulse
	if dOldPos<dNewPos then dDir=-1:DOF 102, DOFPulse

	Dim tmpRotz

	tmpRotz = 77	- 1.125 * (MechPos - 5)
	Drac.rotz = Drac.rotz - (Drac.Rotz - tmpRotz)/20

	If abs(Drac.rotz-tmpRotz) > 0.1 or initDrac = 0 Then
		DracSupport.RotZ=Drac.RotZ:DracSh.RotZ=Drac.RotZ
		For Each xx in DracTargets:xx.IsDropped = 1:Next
		DracTargets(Int((12 + Drac.RotZ)/2+1.25)).IsDropped = 0
		For Each xx in BackSide:xx.IsDropped = 1:Next
		BackSide(Int((12 + Drac.RotZ)/2+1.25)).IsDropped = 0
		initDrac = 1
	End If
End Sub

'*******Drac Shake****************************************
dim dbrake,dshake

Sub DracShake_timer
	if dbrake>0 then			
		dshake=dshake+30
		dbrake=dbrake-0.04
		Drac.transy =((dsin(dshake)))*dbrake
		DracSh.transY=drac.TransY
	else
		dbrake=0:dshake=0:Me.enabled=0
	End if
End Sub

'************************************************************************
'					Switches
'************************************************************************

'Rollovers
Sub sw16_Hit:Controller.Switch(16) = 1:End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub
 
Sub sw17_Hit:Controller.Switch(17) = 1:End Sub
Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub

Sub sw18_Hit:Controller.Switch(18) = 1:End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub

Sub sw26_Hit:Controller.Switch(26) = 1:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub
 
Sub sw27_Hit:Controller.Switch(27) = 1:End Sub
Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub
 
Sub sw56_Hit:Controller.Switch(56) = 1:End Sub
Sub sw56_UnHit:Controller.Switch(56) = 0:End Sub
 
Sub sw57_Hit:Controller.Switch(57) = 1:End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
 
Sub sw58_Hit:Controller.Switch(58) = 1:End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
 
Sub sw61_Hit:Controller.Switch(61) = 1:End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub
 
Sub sw62_Hit:Controller.Switch(62) = 1:End Sub
Sub sw62_UnHit:Controller.Switch(62) = 0:End Sub

Sub sw64_Hit:Controller.Switch(64) = 1:End Sub
Sub sw64_UnHit:Controller.Switch(64) = 0:End Sub

Sub sw65_Hit:Controller.Switch(65) = 1:End Sub
Sub sw65_UnHit:Controller.Switch(65) = 0:End Sub

Sub sw66_Hit:Controller.Switch(66) = 1:End Sub
Sub sw66_UnHit:Controller.Switch(66) = 0:End Sub

' Ramps
Sub sw67_Hit:Controller.Switch(67) = 1:SoundPlayfieldGate:sw67p.rotZ=-25:Me.TimerEnabled=1:End Sub
Sub sw67_timer:Me.TimerEnabled=0:sw67p.rotZ=0:End Sub
Sub sw67_UnHit:Controller.Switch(67) = 0:End Sub

Sub sw68_Hit:vpmTimer.PulseSw 68:End Sub
'Sub sw68_UnHit:Controller.Switch(68) = 0:End Sub

Sub sw71_Hit:vpmTimer.PulseSw 71:End Sub
'Sub sw71_UnHit:Controller.Switch(71) = 0:End Sub

Sub sw72_Hit:Controller.Switch(72) = 1:SoundPlayfieldGate: sw72p.rotZ=25:Me.TimerEnabled=1:End Sub
Sub sw72_timer:Me.TimerEnabled=0:sw72p.rotZ=0:End Sub
Sub sw72_UnHit:Controller.Switch(72) = 0:End Sub

Sub sw73_Hit:Controller.Switch(73) = 1:SoundPlayfieldGate:sw73p.rotZ=25:Me.TimerEnabled=1:End Sub
Sub sw73_timer:Me.TimerEnabled=0:sw73p.rotZ=0:End Sub
Sub sw73_UnHit:Controller.Switch(73) = 0:End Sub

' Gates
Sub sw63_Hit:Controller.Switch(63) = 1:End Sub
Sub sw63_UnHit:Controller.Switch(63) = 0:End Sub

' Opto & proximity switches
Sub sw42_Hit:Controller.Switch(42) = 1:End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub

Sub sw43_Hit:Controller.Switch(43) = 1:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub

Sub sw47_Hit:Controller.Switch(47) = 1:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

Sub sw48_Hit:Controller.Switch(48) = 1:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub

' Spinner
Sub Spinner1_spin():vpmTimer.PulseSw 117:PlaySoundat "fx_spinner", spinner1:End Sub

' Drac Targets
Sub sw12_Hit:STHit 12:End Sub 
Sub sw15_Hit:STHit 15:End Sub

'Tomb Treasure
Sub sw23_Hit:STHit 23:End Sub

' Blue Targets
Sub sw44_Hit:STHit2 sw44, 44:End Sub
Sub sw45_Hit:STHit2 sw45, 45:End Sub
Sub sw46_Hit:STHit2 sw46, 46:End Sub

' #####################################
' ###### Flupper Domes #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.3		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.6		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.1		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "red" : InitFlasher 2, "white" : InitFlasher 3, "white"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
'		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If

	'SetModLamp 123,"0"
'	f23r.IntensityScale = 0 'bumper flasher
'	f19.IntensityScale = 0 'frank tesla flashers
'	f19a.IntensityScale = 0 'frank tesla flashers

End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub


'Iaakki Flasher code modification for frank lights
Const SolDrawSpeed = 1

sub Flashsol19(lvl)
	'debug.print lvl
	dim aLvl : aLvl = lvl / 255

	if aLvl < Objlevel(2) Then	
		Objlevel(2) = Objlevel(2) / SolDrawSpeed
	else 
		Objlevel(2) = aLvl
	end if
	FlasherFlash2_Timer
end sub


VRBGFL19_1.opacity = 0
VRBGFL19_2.opacity = 0
VRBGFL19_3.opacity = 0
VRBGFL19_4.opacity = 0
f19d.opacity = 0
f19e.opacity = 0
f19f.opacity = 0
Flasherflash2.opacity = 0	
Flasherflash3.visible = false
Flasherflash2.opacity = 0	
Flasherflash3.visible = false

f19.intensityscale = 0	
f19a.intensityscale = 0
Flasherlight2.intensityscale = 0	
Flasherlight3.intensityscale = 0
f19.state = 1	
f19a.state = 1	
Flasherlight2.state = 1	
Flasherlight3.state = 1							


Flasherbase2.blenddisablelighting = 0.3
Flasherbase3.blenddisablelighting = 0.3
Flasherbase4.blenddisablelighting = 0.3
Flasherbase5.blenddisablelighting = 0.3	

Flasherlit2.visible = 0
Flasherlit3.visible = 0
Flasherlit4.visible = 0
Flasherlit5.visible = 0

'rotations
flasherlit2.rotz = 130
flasherlit3.rotz = 180
flasherlit4.rotz = 180
flasherlit5.rotz = 180
flasherbase2.rotz = 130
flasherbase3.rotz = 180
flasherbase4.rotz = 180
flasherbase5.rotz = 180

	

Sub FlashFlasher(nr)
	'If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : f23c.visible = 1 :f23d.visible = 1 : f19d.visible = 1 : f19e.visible = 1 : f19f.visible = 1 : End If
	If not objflasher(nr).TimerEnabled Then
		Select Case nr
			Case 1:
				objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1
			Case 2:
				objflasher(nr).TimerEnabled = True 
				'making left visible
				objflasher(nr).visible = 1 : objlit(nr).visible = 1

				'making right visible
				objflasher(3).visible = 1:objlit(3).visible = 1
				'making top lit prims visible
				flasherlit4.visible = 1
				flasherlit5.visible = 1

				f19d.visible = 1
				f19e.visible = 1
				f19f.visible = 1
				If VRFlashingBackglass = 1 then
					VRBGFL19_1.visible = 1
					VRBGFL19_2.visible = 1
					VRBGFL19_3.visible = 1
					VRBGFL19_4.visible = 1
				End If
		End Select
	End If

	select case nr
		Case 1:
			objflasher(nr).opacity = 200 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
			objlight(nr).IntensityScale = 1 * FlasherLightIntensity * ObjLevel(nr)^3
			objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
			objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
			UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
		Case 2:
			'left bottom #2
			objflasher(2).opacity = 1000 *  FlasherFlareIntensity * Objlevel(2)^2.5
			objlight(2).IntensityScale = 1 * FlasherLightIntensity * Objlevel(2)^3
			objbase(2).BlendDisableLighting =  FlasherOffBrightness + 20 * Objlevel(2)^3 + 0.3	
			objlit(2).BlendDisableLighting = 35 * Objlevel(2)^1.2
			UpdateMaterial "Flashermaterial" & 2,0,0,0,0,0,0,Objlevel(2)^1.2,RGB(255,255,255),0,0,False,True,0,0,0,0

			'right Bottom #3
			objflasher(3).opacity = 1000 *  FlasherFlareIntensity * Objlevel(2)^2.5
			objlight(3).IntensityScale = 1 * FlasherLightIntensity * Objlevel(2)^3
			objbase(3).BlendDisableLighting =  FlasherOffBrightness + 20 * Objlevel(2)^3 + 0.3	
			objlit(3).BlendDisableLighting = 35 * Objlevel(2)^1.2
			UpdateMaterial "Flashermaterial" & 3,0,0,0,0,0,0,Objlevel(2)^1.2,RGB(255,255,255),0,0,False,True,0,0,0,0

			'left top #4
			flasherbase4.BlendDisableLighting =  FlasherOffBrightness + 20 * Objlevel(2)^3 + 0.3	
			flasherlit4.BlendDisableLighting = 40 * Objlevel(2)^0.9
			UpdateMaterial "Flashermaterial" & 4,0,0,0,0,0,0,Objlevel(2)^0.9,RGB(255,255,255),0,0,False,True,0,0,0,0

			'right top #5
			flasherbase5.BlendDisableLighting =  FlasherOffBrightness + 20 * Objlevel(2)^3 + 0.3	
			flasherlit5.BlendDisableLighting = 40 * Objlevel(2)^0.9
			UpdateMaterial "Flashermaterial" & 5,0,0,0,0,0,0,Objlevel(2)^0.9,RGB(255,255,255),0,0,False,True,0,0,0,0

			f19d.opacity = 1500 * FlasherFlareIntensity * Objlevel(2)^2	
			f19e.opacity = 500 * FlasherFlareIntensity * Objlevel(2)^2	
			f19f.opacity = 500 * FlasherFlareIntensity * Objlevel(2)^2	
			f19.IntensityScale = 1 * Objlevel(2)^2
			f19a.IntensityScale = 1 * Objlevel(2)^2

			If VRFlashingBackglass = 1 then
				VRBGFL19_1.opacity = 500 * Objlevel(2)^3
				VRBGFL19_2.opacity = 500 * Objlevel(2)^3
				VRBGFL19_3.opacity = 500 * Objlevel(2)^3
				VRBGFL19_4.opacity = 500 * Objlevel(2)^3
			End If

	End Select
	ObjLevel(nr) = ObjLevel(nr) * 0.8 - 0.01
	
	Select case nr
		Case 1: 
			If ObjLevel(nr) < 0 Then
				objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0
			End If
		Case 2: 
			If ObjLevel(nr) < 0 Then
				objflasher(nr).TimerEnabled = False
				objflasher(nr).visible = 0 : objlit(nr).visible = 0	

				'making right non-visible
				objflasher(3).visible = 0 : objlit(3).visible = 0
				'making top lit prims non-visible
				flasherlit4.visible = 0
				flasherlit5.visible = 0

				f19d.visible = 0
				f19e.visible = 0
				f19f.visible = 0
				If VRFlashingBackglass = 1 then
					VRBGFL19_1.visible = 0
					VRBGFL19_2.visible = 0
					VRBGFL19_3.visible = 0
					VRBGFL19_4.visible = 0
				End If
			End If
	End Select
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 

Sub FlashSol23(flstate)
	If Flstate Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
	End If
End Sub


Sub Flashertracker
		If Lightning = 1 then Flasherbloom2.opacity = Flasherflash2.opacity /8
		If Lightning = 1 then Flasherbloom2.visible = Flasherflash2.visible

		f23c.visible = Flasherflash1.visible
		f23c.opacity = Flasherflash1.opacity/2

		f23d.visible = Flasherflash1.visible
		f23d.opacity = Flasherflash1.opacity/2

		f23r.IntensityScale = Flasherlight1.IntensityScale

End Sub

' *********************************************************************
'			GENERAL ILLUMINATION
' *********************************************************************

Set GiCallBack2 = GetRef("UpdateGi")

Sub UpdateGi(nr,step)
    Dim ii
    Select Case nr
    Case 0         'Bottom Playfield
        If step=0 Then
            For each ii in GIBot:ii.state=0:Next
            light11.state=0
        Else
            For each ii in GIBot:ii.state=1:Next
            If VRRoom=0 then light11.state=1: Else light11.state=0:End If
        End If
        Reflect1.Amount = 100 * (1 + step/2)
        Reflect2.IntensityScale = 0.125 * step
		Reflect22.IntensityScale = 0.125 * step	
		igorflash.IntensityScale = 0.125 * step

        light11.IntensityScale = 0.125 * step
        For each ii in GIBot:ii.IntensityScale = 0.125 * step:Next
        If Step>=7 Then Table1.ColorGradeImage = "ColorGrade_8":Else Table1.ColorGradeImage = "ColorGrade_" & (step+1):End If
        
        Primitive17.blenddisablelighting = 0.05 * step
        Primitive19.blenddisablelighting = 0.05 * step
        Primitive20.blenddisablelighting = 0.05 * step
        Primitive21.blenddisablelighting = 0.05 * step


Case 1		'Top Right Playfield
		If step=0 Then
			For each ii in GITopRight:ii.state=0:Next
			For each ii in GIBumpers:ii.state=0:Next
			Reflect001.visible = 0
		Else
			For each ii in GITopRight:ii.state=1:Next
			For each ii in GIBumpers:ii.state=1:Next
			If VRRoom = 0 Then Reflect001.visible = 1
		End If
		BWR.IntensityScale = 0.125 * step
		Reflect5.Amount = 100 * (1 + step/2)
		Reflect6.Amount = 100 * (1 + step/2)
		For each ii in GITopRight:ii.IntensityScale = 0.125 * step:Next
		For each ii in GIBumpers:ii.IntensityScale = 0.125 * step:Next
		Reflect001.IntensityScale = 0.125 * step
		If step>4 Then DOF 103, DOFOn : Else DOF 103, DOFOff:End If
	Case 2		'Top Left Playfield
		If step=0 Then
			For each ii in GITopLeft:ii.state=0:Next
		Else
			For each ii in GITopLeft:ii.state=1:Next
		End If
		If VRRoom=0 or Desktopmode then f18b001.IntensityScale = 0.125 * step: Else f18b001.visible=0:End If
		BWL.IntensityScale = 0.125 * step
		Reflect3.Amount = 100 * (1 + step/2)
		Reflect4.Amount = 100 * (1 + step/2)
		For each ii in GITopLeft:ii.IntensityScale = 0.125 * step:Next
	Case 3		'Top Insert Panel

	Case 4		'Bottom Insert Panel
	End Select
End Sub
 
'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    UpdateLamps
	Flashertracker ' clever shit right here... :D
End Sub

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0         ' current light state, indepEndent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4       ' used to track the fading state
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub UpdateLamps
	NFadeLm 11, l11
	NFadelm 11, l11a
	Flash 11, l11halo
	NFadeLm 12, l12c
	NFadeL 12, l12
	NFadeLm 13, l13
	Flash 13, l13halo
	NFadeLm 14, l14
	Flash 14, l14halo
	NFadeLm 15, l15
	Flash 15, l15halo
	NFadeLm 16, l16
	Flash 16, l16halo
	NFadeLm 17, l17
	Flash 17, l17halo
	NFadeLm 18, l18
	Flash 18, l18halo

	NFadeLm 21, l21
	Flash 21, l21halo
	NFadeLm 22, l22
	Flash 22, l22halo
	NFadeL 23, l23
	NFadeLm 24, l24c
	NFadeLm 24, l24
	Flashm 24, l24halo
	Flash 24, l24chalo
	NFadeLm 25, l25
	Flash 25, l25halo
	NFadeLm 26, l26
	Flash 26, l26halo
	NFadeLm 27, l27
	Flash 27, l27halo
	NFadeLm 28, l28
	Flash 28, l28halo

	NFadeLm 31, l31c
	NFadeL 31, l31
	NFadeLm 32, l32
	Flash 32, l32halo
	NFadeL 33, l33
	NFadeLm 34, l34
	Flash 34, l34halo
	NFadeL 35, l35
	NFadeL 36, l36
	NFadeL 37, l37
	NFadeLm 38, l38
	Flash 38, l38halo
	

	NFadeL 41, l41
	NFadeLm 42, l42
	Flash 42, l42halo
	NFadeLm 43, l43c
	NFadeL 43, l43
	NFadeLm 44, l44
	Flash 44, l44halo
	NFadeLm 45, l45
	Flash 45, l45halo
	NFadeLm 46, l46
	Flash 46, l46halo
	NFadeLm 47, l47
	Flash 47, l47halo
	NFadeLm 48, l48
	Flash 48, l48halo

	NFadeL 51, l51
	NFadeL 52, l52
	NFadeL 53, l53
	NFadeL 54, l54
	NFadeL 55, l55
	NFadeL 56, l56
	NFadeLm 57, l57
	NFadeLm 57, l57a
	Flash 57, l57halo
	NFadeLm 58, l58
	Flash 58, l58halo

	NFadeLm 61, l61
	Nfadel 61, l61a
	
	NFadeLm 62, l62
	Nfadel 62, l62a
	NFadeLm 63, l63
	Nfadel 63, l63a
	NFadeLm 64, l64
	Nfadel 64, l64a
	NFadeLm 65, l65
	Nfadel 65, l65a
	NFadeLm 66, l66
	Nfadel 66, l66a
	NFadeLm 67, l67
	Flash 67, l67halo
	NFadeLm 68, l68
	Flash 68, l68halo

	NFadeL 71, l71

	NFadeL 72, l72
	NFadeLm 73, l73
	Flash 73, l73halo
	NFadeL 74, l74
	NFadeL 75, l75
	NFadeL 76, l76
	NFadeLm 77, l77
	Flash 77, l77halo

	If VRRoom = 0 then
		NFadeLm 81, l81pp
		NFadeLm 81, l81pp2
	End If
	Flashm 81, l81f
	Flashm 81, l81f2
	Flash 81, l81ref
	FadeDisableLighting 81, cfeatures2d, 0.85

	If VRroom = 0 then
		NFadeLm 82, l82pp
		NFadeLm 82, l82pp2
	End If
	Flashm 82, l82f
	Flashm 82, l82f2
	Flash 82, l82ref
	FadeDisableLighting 82, cfeatures2c, 0.85

	If VRroom = 0 then
		NFadeLm 83, l83pp
		NFadeLm 83, l83pp2
	End If
	Flashm 83, l83f
	Flash 83, l83f2
	FadeDisableLighting 83, cfeatures2b, 0.85

	If VRroom = 0 then
		NFadeLm 84, l84pp
		NFadeLm 84, l84pp2
	End If
	Flashm 84, l84f
	Flash 84, l84f2
	FadeDisableLighting 84, cfeatures2a, 0.85

	NFadeLm 85, l85
	NFadeLm 85, l85a
	Flash 85, l85halo
	NFadeLm 86, l86
	NFadeLm 86, l86a
	Flash 86, l86halo

	'*****Flashers*****
	FadeModLamp 117, f17, 1			'WolfMan Flashers
	FadeModLamp 117, f17a, 1
	FadeModLamp 117, f17b, 1
	FadeModLamp 117, VRBGFL17_1, 2	'WolfMan Backglass
	FadeModLamp 117, VRBGFL17_2, 2
	FadeModLamp 117, VRBGFL17_3, 2
	FadeModLamp 117, VRBGFL17_4, 1
	FadeModLamp 118, f18, 2			'Bride Flasher
	FadeModLamp 118, VRBGFL18_1, 2	'Bride Backglass
	FadeModLamp 118, VRBGFL18_2, 2
	FadeModLamp 118, VRBGFL18_3, 2
	FadeModLamp 118, VRBGFL18_4, 1	
	'FadeModLamp 118, f18a, 2

	If VRRoom <> 0 or Desktopmode Then
			FadeModBlend 118, Primitive73, 0.3
		else
			FadeModLamp 118, f18b, 2 
	End If

	FadeModLamp 118, f18r2, 2
	FadeModLamp 118, f18r, 2
'	FadeModLamp 119, f19, 2			'Frank Flashers moved to flupper dome
	FadeModLamp 120, f20, 2	 		'Dracula Flasher
	FadeModLamp 120, f20a, 2	
	FadeModLamp 120, f20b, 2
	FadeModLamp 120, VRBGFL20_1, 2	'Dracula Backglass
	FadeModLamp 120, VRBGFL20_2, 2
	FadeModLamp 120, VRBGFL20_3, 2
	FadeModLamp 120, VRBGFL20_4, 1
	FadeModLamp 121, f21, 2			'Creature Flashers
	FadeModLamp 121, f21a, 2
	FadeModLamp 121, f21b, 2
	FadeModLamp 121, f21c, 2	
	FadeModLamp 122, f22, 2			'Mummy Flashers
	FadeModLamp 122, f22a, 2	
	FadeModLamp 122, f22b, 2	
	FadeModLamp 122, f22c, 2
	FadeModLamp 122, f22d, 2
	FadeModLamp 122, f22e, 2
	FadeModLamp 122, f22f, 2
	FadeModLamp 122, VRBGFL22_1, 2	'Mummy Backglass
	FadeModLamp 122, VRBGFL22_2, 2
	FadeModLamp 122, VRBGFL22_3, 2
	FadeModLamp 122, VRBGFL22_4, 1
	FadeModLamp 122, Primitive42, 1
	'NFadeObjm 123, Flasherbase001, "domeredlit", "domeredbase" Moved to flupper dome
	'FadeModLamp 123, f23a, 2 		'Right Popper Flasher
	'FadeModLamp 123, f23c, 2
	'FadeModLamp 123, f23d, 2	
	'FadeModLamp 123, f23r, 2

	FadeModLamp 124, f24, 2			'Frank Arrow Flasher
	FadeModLamp 125, f25, 2			'Monsters of rock Flasher
	FadeModLamp 125, f25halo, 2
	FadeModLamp 125, VRBGFL25_1, 2	'Monsters of Rock Backglass
	FadeModLamp 125, VRBGFL25_2, 2
	FadeModLamp 125, VRBGFL25_3, 2
	FadeModLamp 125, VRBGFL25_4, 1

	'WolfMan Loop Flashers
	If VRRoom <> 0 or Desktopmode Then
		Flashm 126, f26r3
	else
		Flashm 126, f26r
	End If

	If VRRoom = 0 Then	
		Flashm 126, f26r2
	End If

	NFadeLm 126, f26
	NFadeLm 126, f26a
	Flashm 126, f26halo
	Flash 126, f26ahalo

End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

Sub FadeDisableLighting(nr, a, blend)
    Select Case FadingLevel(nr)
        Case 4:a.blendDisableLighting = 0
        Case 5:a.blendDisableLighting = blend
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub

' Modulated lights and flashers

Sub SetModLamp(nr, value)
    If value > 0 Then
		LampState(nr) = 1
	Else
		LampState(nr) = 0
	End If
	FadingLevel(nr) = value
End Sub

Sub FadeModLamp(nr, object, factor)
	'Object.IntensityScale = FadingLevel(nr) * factor/255
	If TypeName(object) = "Light" Then
		Object.IntensityScale = FadingLevel(nr) * factor/255
		Object.State = LampState(nr)
	End If
	If TypeName(object) = "Flasher" Then
		Object.IntensityScale = FadingLevel(nr) * factor/255
		Object.visible = LampState(nr)
	End If
	If TypeName(object) = "Primitive" Then
		Object.blenddisablelighting = FadingLevel(nr) * factor/255
	End If
End Sub

Sub FadeModBlend(nr, object, factor)
	Object.blenddisablelighting = FadingLevel(nr) * factor/255
End Sub

'*****************
' Maths
'*****************
Const Pi = 3.1415927

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

' *********************************************************************
' 					Ball Drop & Ramp Sounds
' *********************************************************************
'center ramp
Sub CREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtBall "fx_lrenter":End If:End Sub			'ball is going up
Sub CREnter_UnHit():If ActiveBall.VelY > 0 Then WireRampOff:StopSound "fx_lrenter":End If:End Sub		'ball is going down
Sub CRExit_Hit:StopSound "fx_lrenter" : WireRampOn False, "CRExit": End Sub ' Me.TimerInterval=100:Me.TimerEnabled=1:End Sub
'Sub CRExit_Timer(): Me.TimerEnabled=0 : WireRampOn False, "CRExit" : End Sub
'left ramp
Sub LREnter_hit():WireRampOn False, "LREnter":If Activeball.VelY<6 Then ActiveBall.VelY=12:End If:End Sub
Sub LRExit_Hit:WireRampOff:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub LRExit_timer:Me.TimerEnabled=0:PlaySoundAt "fx_wireramp_exit", LRExit::End Sub
'right ramp
Sub RREnter_Hit():If ActiveBall.VelY < 0 Then PlaySoundAtBall "fx_lrenter":End If:End Sub			'ball is going up
Sub RREnter_UnHit():If ActiveBall.VelY > 0 Then StopSound "fx_lrenter":End If:End Sub		'ball is going down
Sub RRail_Hit::If ActiveBall.VelY < 0 Then StopSound "fx_lrenter" : WireRampOn False, "RRail":End If: End Sub' Me.TimerInterval=100:Me.TimerEnabled=1:End If:End Sub		'ball is going up
Sub RRail_UnHit:If ActiveBall.VelY > 0 Then WireRampOff:End If:End Sub		'ball is going down
'Sub RRail_Timer(): Me.TimerEnabled=0 : WireRampOn False, "RRail" : End Sub
Sub RRPost_Hit():WireRampOff:End Sub
Sub RRPost_UnHit():me.Enabled = 0:PlaySoundAtBall "fx_wireramp_exit":End Sub
Sub RRExit_Hit:WireRampOff:Me.TimerInterval=200:Me.TimerEnabled=1:End Sub
Sub RRExit_timer:Me.TimerEnabled=0:PlaySoundAt "fx_wireramp_exit", RRExit::End Sub

'******************************************************
'       	RealTime Updates
'******************************************************
'Set MotorCallback = GetRef("RealTimeUpdates")

Sub GameTimer_Timer()
	RollingSoundUpdate
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows	
	DracUpdate
	If ShakeEnabled = 0 then creature.rotz = (ckicker.y - cball.y) / 8
	sw68p.RotX = GateLR.currentangle
	sw71p.RotX = GateRR.currentangle
	sw63p.RotX = GateR.currentangle
	LeftFlipperSh.RotZ = LeftFlipper.currentangle
	RightFlipperSh.RotZ = RightFlipper.currentangle
	LeftGateP.RotY = -LGate.currentangle*2/3
	RightGateP.RotY = -RGate.currentangle*2/3
	If gateLR.currentangle>75 Then sw68p1.RotX=-10 Else sw68p1.RotX=0
	If gateRR.currentangle>75 Then sw71p1.RotX=-10 Else sw71p1.RotX=0
	If gateR.currentangle>75 Then sw63p1.RotX=-10 Else sw63p1.RotX=0

	DoSTAnim
	Cor.Update
End Sub

'*********** ROLLING SOUND *********************************
Const tnob = 6						' total number of balls : 4 (trough) + 1 (Fake Ball)
ReDim rolling(tnob)
InitRolling

Dim ampFactor

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
	Select Case BallRollAmpFactor
		Case 0
			ampFactor = "_amp0"
		Case 1
			ampFactor = "_amp2_5"
		Case 2
			ampFactor = "_amp5"
		Case 3
			ampFactor = "_amp7_5"
		Case 4
			ampFactor = "_amp9"
		Case Else
			ampFactor = "_amp0"
	End Select
End Sub
'
Dim DropCount
'BallShadow = Array (BallShadow1, BallShadow2, BallShadow3,Ballshadow4, BallShadow5, BallShadow6)
ReDim DropCount(tnob)

Sub RollingSoundUpdate()
	Dim BOT, b
	BOT = GetBalls

	For b = 0 to UBound(BOT)
	' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1
		End If

		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 and not (BOT(b).x < 175 and  BOT(b).y > 1900) Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b & ampFactor), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b & ampFactor)
				rolling(b) = False
			End If
		End If

		'*** Left Ramp Helper ***	
		If BOT(b).z < 65 and InRect(BOT(b).x, BOT(b).y, 275, 162, 275, 282, 375, 282, 375, 162) Then
			If ABS(BOT(b).vely) < 1 then BOT(b).vely = -4
		End If

		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				'DropCount(b) = 0
				If BOT(b).velz > -5 Then
					If BOT(b).z < 35 Then
						DropCount(b) = 0
						RandomSoundBallBouncePlayfieldSoft BOT(b)
					End If
				Else
					DropCount(b) = 0
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
	Next
End Sub

'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

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

'******************************************************
'			FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
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

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
	
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'		FLIPPER POLARITY AND RUBBER DAMPENER
'			SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

' Used for drop targets and flipper tricks
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

'******************************************************
'			FLIPPER TRICKS
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
        Dim BOT, b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
                If Flipper2.currentangle = EndAngle2 Then 
                        BOT = GetBalls
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
                                        'Debug.Print "ball in flip1. exit"
                                         exit Sub
                                end If
                        Next
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
                                        BOT(b).velx = BOT(b).velx / 1.5
                                        BOT(b).vely = BOT(b).vely - 0.5
'										debug.print "nudge"
                                end If
                        Next
                End If
        Else 
			If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then 
				EOSNudge1 = 0
			end if
        End If
End Sub

'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************

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

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 
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
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

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
		Dim BOT, b
		BOT = GetBalls
			
		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper

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
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, Rubberizer
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub


'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

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
    elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

'****************************************************************************
'PHYSICS DAMPENERS

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


dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

'######################### Add Dampenf to Dampener Class 
'#########################    Only applies dampener when abs(velx) < 2 and vely < 0 and vely > -3.75  


Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm, ver)
		If ver = 1 Then
			dim RealCOR, DesiredCOR, str, coef
			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
			End If
		Elseif ver = 2 Then
			If parm < 10 And parm > 2 And Abs(aball.angmomz) < 15 And aball.vely < 0 then
				aball.angmomz = aball.angmomz * 1.2
				aball.vely = aball.vely * 1.2
			Elseif parm <= 2 and parm > 0.2 And aball.vely < 0 Then
				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        	aball.angmomz = aball.angmomz * -0.7
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				aball.vely = aball.vely * 1.4
			End if
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report() 	'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub
	

End Class
'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

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
'		STAND-UP TARGET INITIALIZATION
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
Dim ST12, ST15 , ST85, ST86, ST23

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0

Set ST12 = (new StandupTarget)(sw12, sw12p,12, 0)
Set ST15 = (new StandupTarget)(sw15, sw15p,15, 0)
Set ST23 = (new StandupTarget)(sw23, sw23p,23, 0)
Set ST85 = (new StandupTarget)(sw85, frankytargets,85, 0)
Set ST86 = (new StandupTarget)(sw86, frankytargets,86, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST12, ST15, ST85, ST86, ST23)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  0.5				'vpunits per animation step (control return to Start)
Const STMaxOffset = 6				'max vp units target moves when hit
Const STHitSound = "fx_target"	'Stand-up Target Hit sound

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

	'PlayTargetSound
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Sub STHit2(target, switch)
	'PlayTargetSound
	If STCheckHit(Activeball,target) = 1 Then
		vpmTimer.PulseSw switch
	End If
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).sw = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 

	If perpvel <= 0 or perpvelafter >= 0 Then
		STCheckHit = 0 
	Else
		STCheckHit = 1
	End If
End Function


Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
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

' Used for drop targets and stand up targets
Function Atn2(dy, dx)
	dim pi
	pi = 4*Atn(1)

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

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

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


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
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
	If ball.z > 34 Then
		Volz = Csng(20^2)
	else
		Volz = Csng((ball.velz) ^2)
	End If
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
	End Select
End Sub

Sub SoundNudgeRight()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
	End Select
End Sub

Sub SoundNudgeCenter()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
	End Select
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
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic ("Drain_1"), DrainSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic ("Drain_2"), DrainSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic ("Drain_3"), DrainSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic ("Drain_4"), DrainSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic ("Drain_5"), DrainSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic ("Drain_6"), DrainSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic ("Drain_7"), DrainSoundLevel, drainswitch
		Case 8 : PlaySoundAtLevelStatic ("Drain_8"), DrainSoundLevel, drainswitch
		Case 9 : PlaySoundAtLevelStatic ("Drain_9"), DrainSoundLevel, drainswitch
		Case 10 : PlaySoundAtLevelStatic ("Drain_10"), DrainSoundLevel, drainswitch
		Case 11 : PlaySoundAtLevelStatic ("Drain_11"), DrainSoundLevel, drainswitch
	End Select
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBallRelease(drainswitch)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("BallRelease1",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic SoundFX("BallRelease2",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic SoundFX("BallRelease3",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic SoundFX("BallRelease4",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic SoundFX("BallRelease5",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic SoundFX("BallRelease6",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic SoundFX("BallRelease7",DOFContactors), BallReleaseSoundLevel, drainswitch
	End Select
End Sub



'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_L1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_L2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_L3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_L4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_L5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_L6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_L7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_L8",DOFContactors), SlingshotSoundLevel, Sling
		Case 9 : PlaySoundAtLevelStatic SoundFX("Sling_L9",DOFContactors), SlingshotSoundLevel, Sling
		Case 10 : PlaySoundAtLevelStatic SoundFX("Sling_L10",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

Sub RandomSoundSlingshotRight(sling)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_R1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_R2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_R3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_R4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_R5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_R6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_R7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_R8",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperMiddle(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperBottom(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
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
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_L01",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_L02",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_L07",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_L08",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_L09",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_L10",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_L12",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_L14",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_L18",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_L20",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_L26",DOFFlippers), FlipperLeftHitParm, Flipper
	End Select
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_R01",DOFFlippers), FlipperRightHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_R02",DOFFlippers), FlipperRightHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_R03",DOFFlippers), FlipperRightHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_R04",DOFFlippers), FlipperRightHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_R05",DOFFlippers), FlipperRightHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_R06",DOFFlippers), FlipperRightHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_R07",DOFFlippers), FlipperRightHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_R08",DOFFlippers), FlipperRightHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_R09",DOFFlippers), FlipperRightHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_R10",DOFFlippers), FlipperRightHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_R11",DOFFlippers), FlipperRightHitParm, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpRight(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_8",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_1"), parm  * RubberFlipperSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_2"), parm  * RubberFlipperSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_3"), parm  * RubberFlipperSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_4"), parm  * RubberFlipperSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_5"), parm  * RubberFlipperSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_6"), parm  * RubberFlipperSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_7"), parm  * RubberFlipperSoundFactor
	End Select
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rollover_1"), RolloverSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Rollover_2"), RolloverSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Rollover_3"), RolloverSoundLevel
		Case 4 : PlaySoundAtLevelActiveBall ("Rollover_4"), RolloverSoundLevel
	End Select
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
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor
	End Select
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then
 		RandomSoundRubberStrong 1 
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
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
	Select Case Int(Rnd*13)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Metal_Touch_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Metal_Touch_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Metal_Touch_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Metal_Touch_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Metal_Touch_5"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Metal_Touch_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Metal_Touch_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Metal_Touch_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Metal_Touch_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall ("Metal_Touch_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall ("Metal_Touch_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall ("Metal_Touch_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall ("Metal_Touch_13"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
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
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_2"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_1"), BottomArchBallGuideSoundFactor * 0.25
		Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_2"), BottomArchBallGuideSoundFactor * 0.25
		Case 3 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_3"), BottomArchBallGuideSoundFactor * 0.25
	End Select
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
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Medium_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*7)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Soft_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Soft_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Soft_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall ("Apron_Soft_4"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall ("Apron_Soft_5"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 6 : PlaySoundAtLevelActiveBall ("Apron_Soft_6"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 7 : PlaySoundAtLevelActiveBall ("Apron_Soft_7"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_5",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_6",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_7",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_8",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
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
	TargetBouncer Activeball, 1
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_3"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_4"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_6"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
	End Select
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
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_FastTrigger_1"), GateSoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_FastTrigger_2"), GateSoundLevel, Activeball
	End Select
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
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_L1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_L2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_L3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_L4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub

Sub RandomSoundRightArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_R1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_R2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_R3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_R4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
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
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, Activeball
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, Activeball
	End Select
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

'////////////////////////Options////////////////////////

DIM VRThings


If CabinetMode = 1 Then
		Ramp15.visible = False
		Ramp16.visible = False
		LeftCab.Size_Z = 20
		RightCab.Size_Z = 20
	Else
		Ramp15.visible = True
		Ramp16.visible = True
		LeftCab.Size_Z = 1
		RightCab.Size_Z = 1
End If


If VRRoom > 0 Then
	Scoretext.visible = 0
	F18b.visible = 0
	f18b001.visible =0
	igorflash.visible = 0
	light11.state = 0
	primitive28.material = "plastic"
	primitive1.material = "plastic"
	l81ref.opacity = 2
	l82ref.opacity = 2
	f26r.visible = 0
	f26r2.visible = 0
	f26r3.visible = 1
	PinCab_Backglass.blenddisablelighting = 3
'	Ramp15.visible = 0
'	Ramp16.visible = 0
	for each VRThings in PegsVR:VRThings.visible = 1:Next
	for each VRThings in PegsNormal:VRThings.visible = 0:Next
	If VRRoom = 1 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VR360:VRThings.visible = 1:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		for each VRThings in Desktop:VRThings.visible = 0:Next
		for each VRThings in Reflect:VRThings.visible = 0:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VR360:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 1:Next
		for each VRThings in Desktop:VRThings.visible = 0:Next
		for each VRThings in Reflect:VRThings.visible = 0:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VR360:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		for each VRThings in Desktop:VRThings.visible = 0:Next
		for each VRThings in Reflect:VRThings.visible = 1:Next
		PinCab_Backglass.visible = 1
		PinCab_Rails.visible = 1
		PinCab_Rails.rotx = 91.3
		PinCab_Rails.z = -20
		DMD1.visible = 1
	End If
	If VRFlashingBackglass = 1 Then
		BGDark.visible = 1
		SetBackglass
		Pincab_Backglass.visible = 0
		For each VRThings in VRBGGI:VRThings.visible = 1: Next
	End If
Else
		for each VRThings in PegsVR:VRThings.visible = 0:Next
		for each VRThings in PegsNormal:VRThings.visible = 1:Next
		for each VRThings in VR360:VRThings.visible = 0:Next
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		for each VRThings in Reflect:VRThings.visible = 1:Next
		For each VRThings in VRBGGI:VRThings.visible = 0:Next
		f26r3.visible = 0
	If DesktopMode Then
	F18b.visible = 0
	f18b001.visible =0
	igorflash.visible = 0
	f26r.visible = 0
	f26r3.visible = 1
	End If
End if




'*** Setup VR Backglass Flashers ***
Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x + 3
		obj.height = - obj.y + 280
		obj.y = -165 'adjusts the distance from the backglass towards the user (+ toward user)
		obj.rotx=-86.5
	Next
End Sub


'*****************************************************************************************************************************************
'  ERROR LOGS by baldgeek
'*****************************************************************************************************************************************

' Log File Usage:
'   WriteToLog "Label 1", "Message 1 "
'   WriteToLog "Label 2", "Message 2 "

Const KeepLogs = False					 

Class DebugLogFile

    Private Filename
    Private TxtFileStream
    
    Private Function LZ(ByVal Number, ByVal Places)
        Dim Zeros
        Zeros = String(CInt(Places), "0")
        LZ = Right(Zeros & CStr(Number), Places)
    End Function

    Private Function GetTimeStamp
        Dim CurrTime, Elapsed, MilliSecs
        CurrTime = Now()
        Elapsed = Timer()
        MilliSecs = Int((Elapsed - Int(Elapsed)) * 1000)
        GetTimeStamp = _
            LZ(Year(CurrTime),   4) & "-" _
            & LZ(Month(CurrTime),  2) & "-" _
            & LZ(Day(CurrTime),    2) & " " _
            & LZ(Hour(CurrTime),   2) & ":" _
            & LZ(Minute(CurrTime), 2) & ":" _
            & LZ(Second(CurrTime), 2) & ":" _
            & LZ(MilliSecs, 4)
    End Function

' *** Debug.Print the time with milliseconds, and a message of your choice
    Public Sub WriteToLog(label, message, code)
        Dim FormattedMsg, Timestamp
        'Filename = UserDirectory + "\" + cGameName + "_debug_log.txt"
		Filename = cGameName + "_debug_log.txt"
      
        Set TxtFileStream = CreateObject("Scripting.FileSystemObject").OpenTextFile(Filename, code, True)
        Timestamp  = GetTimeStamp     
        FormattedMsg = GetTimeStamp + " : " + label + " : " + message
        TxtFileStream.WriteLine FormattedMsg
        TxtFileStream.Close
		debug.print label & " : " & message
	End Sub

End Class

Sub WriteToLog(label, message)
	if KeepLogs Then
		Dim LogFileObj
		Set LogFileObj = New DebugLogFile
		LogFileObj.WriteToLog label, message, 8
	end if
End Sub

Sub NewLog()
	if KeepLogs Then
		Dim LogFileObj
		Set LogFileObj = New DebugLogFile
		LogFileObj.WriteToLog "NEW LOG", " ", 2
	end if
End Sub

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
'- To stop tracking ball, use WireRampoff
'--	Otherwise, the ball will auto remove if it's below 30 vp units
'
'Example:
'Sub RampSoundPlunge1_hit() : WireRampOn  False, "LeftRamp" : End Sub			' Wire Ramp Sound
'Sub RampSoundPlunge2_hit() : WireRampOn True, "CenterRamp" : End Sub			' Plastic Ramp Sound
'Sub RampSoundPlunge3_hit() : WireRampOnSndX False, 2, "RightRamp" : End Sub    ' Wire Ramp Sound volume multiplication of 2
'Sub RampEntry_Hit() : If activeball.vely < -10 then WireRampOn True : End Sub 	'Ramp enterance

dim RampMinLoops : RampMinLoops = 4
dim rampAmpFactor

InitRampRolling

Sub InitRampRolling()
	Select Case RampRollAmpFactor
		Case 0
			rampAmpFactor = "_amp0"
		Case 1
			rampAmpFactor = "_amp2_5"
		Case 2
			rampAmpFactor = "_amp5"
		Case 3
			rampAmpFactor = "_amp7_5"
		Case 4
			rampAmpFactor = "_amp9"
		Case Else
			rampAmpFactor = "_amp0"
	End Select
End Sub

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'                    Set BallSndMultiplyer and RampID to be same as RampBalls or tnob + 1
dim RampBalls(7,2)
dim BallSndMultiplyer(7)
dim RampID(7)

'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(7)

Sub WireRampOnSndX(input, sndMultiplier, rmp)  : Waddball ActiveBall, input, sndMultiplier, rmp : RampRollUpdate: End Sub
Sub WireRampOn(input, rmp)  : Waddball ActiveBall, input, 1, rmp : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub

' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput, sndMultiplier, rampname)	'Add ball
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
			BallSndMultiplyer(x) = sndMultiplier
			RampID(x) = rampname
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
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
			BallSndMultiplyer(x) = 1
			RampID(x) = Empty
			StopSound("RampLoop" & x & rampAmpFactor)
			StopSound("wireloop" & x & rampAmpFactor)
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
				WriteToLog "1 Ramp " & RampID(x), "Volume = " & VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial
				WriteToLog "2 Ramp " & RampID(x), "Volume = " & VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial * BallSndMultiplyer(x)
				If RampType(x) then
					PlaySound("RampLoop" & x & rampAmpFactor), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial * BallSndMultiplyer(x), AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x & rampAmpFactor)
				Else
					StopSound("RampLoop" & x & rampAmpFactor)
					PlaySound("wireloop" & x & rampAmpFactor), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial * BallSndMultiplyer(x), AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x & rampAmpFactor)
				StopSound("wireloop" & x & rampAmpFactor)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x & rampAmpFactor)
				StopSound("wireloop" & x & rampAmpFactor)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x & rampAmpFactor)
			StopSound("wireloop" & x & rampAmpFactor)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

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


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by iaakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection
' The easiest way to keep track of this is to start with the group on the left slingshot and move clockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'																E
'	A		 C													B
'	 B		D			your collection should look like		A		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F													C
'																D
'																F
'
'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
'									'2 = flasher image shadow, but it moves like ninuzzu's
Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.98	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
'	' stop the sound of deleted balls
'	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		rolling(b) = False
'		StopSound("BallRoll_" & b)
'	Next
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then 
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function
'
'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		iii = iii + 1
	Next
	numberofsources = iii
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, BOT, iii
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).size_x = 6.5 * ((BOT(s).Z-(ballsize/2))/70)
				objBallShadow(s).size_y = 5.0 * ((BOT(s).Z-(ballsize/2))/70)
				objBallShadow(s).visible = 1

				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + fovY
				objBallShadow(s).size_x = 5.5
				objBallShadow(s).size_y = 4.0
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = BOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((BOT(s).x-DSSources(iii)(0)),(BOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff And light20.state > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							AnotherSource = sourcenames(s)
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((BOT(s).x-DSSources(AnotherSource)(0)),(BOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
							End If
						end if
					Else
						currentShadowCount(s) = 0
						BallShadowA(s).Opacity = 100*AmbientBSFactor
					End If
				Next
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by iaakki, Apophis, and Wylte
'****************************************************************

Sub Flasherbase4_Hit()
	
End Sub

' 1.023 - fluffhead35 - Updated in Rubberizer, Fixed VR Code, Updatged FlipperNudge, Added Target Bouncer Code. Imported Ball_roll Amp SoundSaucerKick
' RC1 - iaakki - Dynamic ball shadows added
' RC2 - Skitso - latest lighting improvements to both GI and inserts.
' RC3 - Rothbauerw - cleaned rolling sound sub, adjusted ramp switches, added protective walls, fixed dynamic shadows, Added in conditional checks for desktop for igorflash and fb18 flashers, added variability to the creature feature
' RC4 - Skitso - Improved frank, mummy and popper flashers.
' RC5 - Leojreimroc - Modulated flashers for Frankenstein flashers.  VR flashing Backglass.
' RC6 - tomate - new WireRamps textures added
' RC7 - iaakki - tuned flasherlit image and fading speeds for franken domes
' RC8 - iaakki - frank flashers redone from scratch
' RC9 - Skitso - Further improvements to Frank flashers. Remade GI lighting under Frank. Remade Bride flasher, Remade Dracula flasher, improved creature flasher, tweaked wolfman and mummy flashers (back wall), fixed and improved numerous side blade reflections.
' RC9.1 - Skitso - Further flasher balancing. Improved top lane guide lighting. Fixed GI bulbs to correct size (was set to 1, and thus they were practically invisible). Fixed too transparent plastic material. Improved GI lighting to fix how light travels to plastics. (top corners and creature plastic)
' RC9.2 - apophis - Updated FadeModLamp. Added Primitive42 to mummy flasher. DL value might need tuning.
' RC9.3 - Leojreimroc - Reworked VRBackglass flashers.  Wall added under the apron for VR.  Took out some lights for VR (Creature lights, wolfman loop flashers)
' RC9.4 - Sixtoe - Loads of little tweaks, set quite a few things to non-collidable, fixed a could of VR issues.
' RC9.5 - rothbauerw - added wall underplayfield, fixed stuck balls from PinStratDan
' RC9.6 - rothbauerw - minor fix to plungerlane
' RC9.7 - iaakki - Flasher dome script optimization. Dyn ball shadow typo fix. Dyn shadows level 0.95 -> 0.98
' RC9.8 - Skitso - Improved mummy flasher, added light travel to plunger lane blue bulbs so they lit up the transparent plastic above them, removed AA default, set post difficulty to middle as a default, one last overall flasher balance tweak, made ball tiny bit darker
' RC9.9 - Rothbauerw - Fixed phantom flip proximity sensors
' RC10 - Skitso - fixed green creature bulbs side blade reflections, tweaks to POV 
