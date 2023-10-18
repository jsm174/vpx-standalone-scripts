' Elvis - Stern 2004
' MOD 2.0 by chokeee (original table by JPsalas from 2017)

Option Explicit
Randomize

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'----- Physics Mods -----
Const FlipperCoilRampupMode = 0   	' 0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		' 0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	' Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled

const PWMflashers = true ' requires 2023 pinmame beta

Const VegasMod = 0 'Las Vegas Mod 1=Show 0=Hide

Const BallSize = 50
Const BallMass = 1
Const BounceMin = 4	
Const BounceMax = 7

'----- VR Room Auto-Detect & Trim Options -----
Dim VRRoom, VR_Obj, GoldTrim

GoldTrim = 1 ' 0 = Standard Trim  1 = Gold Trim (Stern Limited Edition Model - Rails/Legs/Lockbar/Speakers)

If RenderingMode = 2 Then
	VRRoom = 1
	For Each VR_Obj in VR : VR_Obj.Visible = 1 : Next

	If GoldTrim = 1 Then
		PinCab_Trim.Material = "Metal Wire Gold"
		PinCab_Trim.ReflectionEnabled = True
		PinCab_Grills.Material = "Metal Gold Dark"
	End If

	sidewall_right.Sidevisible = false
    sidewall_left.Sidevisible = false
	lrail.Visible = 0
	rrail.Visible = 0
	table1.PlayfieldReflectionStrength = 10
	Light28.Intensity = 0
	gi44.Intensity = 5 : gi45.Intensity =  5: gi001.Intensity = 5
	gi46.Intensity = 5 : gi006.Intensity = 5 : gi002.Intensity = 5
	gi_001.Intensity = 5 : gi_006.Intensity = 5 : gi_005.Intensity = 5
	gi2.Intensity = 5 : gi10.Intensity = 5 : gi14.Intensity = 5 : gi27.Intensity = 5
	gi_39.Intensity = 5 : gi_004.Intensity = 5 : gi_003.Intensity = 5
'	gi8.Intensity = 5 : gi9.Intensity = 5 : gi13.Intensity = 5 : gi11.Intensity = 5
Else
	VRRoom = 0
	For Each VR_Obj in VR : VR_Obj.Visible = 0 : Next
End If


'----- Vegas Mod -----

If VegasMod = 1 then 

	vegas1.visible = 1 
	vegas2.visible = 1
	vegas3.visible = 1
	vegas4.visible = 1
	vegas5.visible = 1
	vegas6.visible = 1
	vegas7.visible = 1
	vegas8.visible = 1
	vegas9.visible = 1
	vegas10.visible = 1

  
else 

	vegas1.visible = 0 
	vegas2.visible = 0
	vegas3.visible = 0
	vegas4.visible = 0
	vegas5.visible = 0
	vegas6.visible = 0
	vegas7.visible = 0
	vegas8.visible = 0
	vegas9.visible = 0
	vegas10.visible = 0

End if

'----- End Vegas Mod -----


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

' Language Roms
Const cGameName = "elvis" 'English

Dim VarHidden, UseVPMColoredDMD

If Table1.ShowDT = true then
	Pincab_Trim.Visible = 1

	If GoldTrim = 1 Then
		PinCab_Trim.Material = "Metal Wire Gold"
	End If

    UseVPMColoredDMD = true
    VarHidden = 1
else
    UseVPMColoredDMD = False
    VarHidden = 0
    TextBox1.Visible = 0
end if


dim UseVPMModSol : UseVPMModSol = cBool(PWMflashers)						   

LoadVPM "01560000", "SEGA.VBS", 3.26

If PWMflashers and Controller.Version < "03060000" Then msgbox "VPinMAME ver 3.6 beta or later is required. Or set script option PWMflashers = False"

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_Coin"

Dim bsTrough, mMag, mCenter, bsLock, bsUpper, plungerIM, mElvis, x, i   'dtBankL, bsJail

'************
' Table init.
'************


'1001 to 1032 for solenoids, 1201 to 1025 for GIs (WPC only), 1301+ for lamps (not yet implemented)
Const VPM_MODOUT_DEFAULT              =   0 ' Uses default driver modulated solenoid implementation
Const VPM_MODOUT_PWM_RATIO            =   1 ' pulse ratio over the last integration period, allow (approximated) device emulation by the calling app
Const VPM_MODOUT_BULB_44_6_3V_AC      = 100 ' Incandescent #44/555 Bulb connected to 6.3V, commonly used for GI
Const VPM_MODOUT_BULB_47_6_3V_AC      = 101 ' Incandescent #47 Bulb connected to 6.3V, commonly used for (darker) GI with less heat
Const VPM_MODOUT_BULB_86_6_3V_AC      = 102 ' Incandescent #86 Bulb connected to 6.3V, seldom use: TZ, CFTBL,...
Const VPM_MODOUT_BULB_44_18V_DC_WPC   = 201 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_44_18V_DC_GTS3  = 202 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_44_18V_DC_S11   = 203 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_89_20V_DC_WPC   = 301 ' Incandescent #89/906 Bulb connected to 12V, commonly used for flashers
Const VPM_MODOUT_BULB_89_20V_DC_GTS3  = 302 ' Incandescent #89/906 Bulb connected to 12V, commonly used for flashers
Const VPM_MODOUT_BULB_89_32V_DC_S11   = 303 ' Incandescent #89/906 Bulb connected to 32V, used for flashers on S11 with output strobing
Const VPM_MODOUT_LED                  = 400 ' LED PWM (in fact mostly human eye reaction, since LED are nearly instantaneous)

Sub InitPWM() ' called from Table_Init
	dim BulbType : BulbType = VPM_MODOUT_BULB_89_20V_DC_WPC
'	dim BulbType : BulbType = VPM_MODOUT_LED

	Controller.SolMask(1020) = BulbType ' top left, top right flashers (caps)
	Controller.SolMask(1021) = BulbType ' top center flasher (caps)
	Controller.SolMask(1022) = BulbType ' left middle flasher (caps)
	Controller.SolMask(1023) = BulbType 'right pf flasher
	Controller.SolMask(1031) = BulbType ' center pf flasher
	Controller.SolMask(1032) = BulbType ' left, right slingshot flashers (caps)
end Sub

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        .Games(cGameName).Settings.Value("sound") = 1 'enable the rom sound
        .SplashInfoLine = "Elvis - Stern 2004" & vbNewLine & "VPX table by JPSalas v.1.0.2"
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .Hidden = VarHidden
        .Games(cGameName).Settings.Value("rol") = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description

        On Error Goto 0
    End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    vpmNudge.TiltSwitch = 56
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 14, 13, 12, 11, 0, 0, 0
        .InitKick BallRelease, 90, 4
'        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 4
    End With

    ' Magnet
    Set mMag = New cvpmMagnet
    With mMag
        .InitMagnet Magnet, 60
        .Solenoid = 5
        .GrabCenter = 1
        '.CreateEvents "mMag"
    End With

'    ' Droptargets
'    set dtBankL = new cvpmdroptarget
'    With dtBankL
'        .initdrop array(sw17, sw18, sw19, sw20, sw21), array(17, 18, 19, 20, 21)
''        .initsnd SoundFX("fx_droptarget", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
'    End With

    ' Hotel Lock
    Set bsLock = New cvpmBallStack
    With bsLock
        .InitSw 0, 48, 0, 0, 0, 0, 0, 0
        .InitKick HLock, 180, 8
'        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    End With

'    ' Jail Lock
'    Set bsJail = new cvpmBallStack
'    With bsJail
'        .InitSaucer sw34, 34, 186, 23
''        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
'        .KickAngleVar = 1
'        .KickForceVar = 1
'    End With

    ' Upper Lock
    Set bsUpper = new cvpmBallStack
    With bsUpper
        .InitSaucer sw32, 32, 90, 15
'        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .KickAngleVar = 3
        .KickForceVar = 3
    End With

    ' Impulse Plunger
    Const IMPowerSetting = 60 ' Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .switch 16
'        .InitExitSnd SoundFX("fx_autoplunger", DOFContactors), SoundFX("fx_autoplunger", DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Elvis movement
    Set mElvis = New cvpmMech
    With mElvis
        .MType = vpmMechOneSol + vpmMechReverse + vpmMechLinear
        .Sol1 = 25
        .Length = 150
        .Steps = 150
        .AddSw 33, 0, 0
        .Callback = GetRef("UpdateElvis")
        .Start
    End With
    ' Initialize Elvis arms and legs
    ElvisArms 0
    ElvisLegs 0
	' Initialize beta PWM
	if UseVPMModSol then InitPWM()		  
End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_Exit:Controller.Stop:End Sub


sub Magnet_Hit() 
	mMag.AddBall activeball
'	PlaySound "fx_magnet_catch"
End Sub

sub Magnet_UnHit()
	if mMag.MagnetOn then 
		activeball.vely = activeball.vely * -0.8  ' TZ hack for unreliable magnets
		activeball.velx = activeball.velx * -0.8
	else 
		mMag.removeball activeball
	end If
End Sub


'****
'Keys
'****

DIm BIPL : BIPL=0

Sub Table1_KeyDown(ByVal keycode)	
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	If keycode = LeftTiltKey Then Nudge 90, 2:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 2:SoundNudgeRight()
    If keycode = CenterTiltKey Then Nudge 0, 2:SoundNudgeCenter()

    If KeyCode = PlungerKey Then
		Plunger.Pullback
		SoundPlungerPull()
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

    If keyUpperLeft Then Controller.Switch(55) = 1
'   If keycode = KeyRules Then Rules
	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
                End Select
    End If
    	if keycode=StartGameKey then soundStartButton()
		If KeyDownHandler(KeyCode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)	
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
    If KeyUpHandler(KeyCode) Then Exit Sub
    If keyUpperLeft Then Controller.Switch(55) = 0
	If KeyCode = PlungerKey Then
		Plunger.Fire
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		PinCab_Shooter.Y = -73.66
		If BIPL = 1 Then
			SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
		End If
	End If
End Sub


'**********************
'Elvis movement up/down
'**********************

Sub UpdateElvis(aNewPos, aSpeed, aLastPos)
    pStand.x = 518+aNewPos/3
    pLarm.x = 479+aNewPos/3
    pLegs.x = 515+aNewPos/3
    pRarm.x = 539+aNewPos/3
    pBody.x = 505+aNewPos/3
    pStand.y = 482+aNewPos
    pLarm.y = 502+aNewPos
    pLegs.y = 500+aNewPos
    pRarm.y = 487+aNewPos
    pBody.y = 471+aNewPos
End Sub


'********************
'     Flippers
'********************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"


Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
    If Enabled Then
        LF.Fire
        leftflipper1.RotateToEnd
        If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
            RandomSoundReflipUpLeft LeftFlipper
        Else 
            SoundFlipperUpAttackLeft LeftFlipper
            RandomSoundFlipperUpLeft LeftFlipper
        End If
    Else
        LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
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

Sub SolURFlipper(Enabled)
    If Enabled Then
        rightflipper1.RotateToEnd

        If rightflipper1.currentangle > rightflipper1.endangle - ReflipAngle Then
            RandomSoundReflipUpRight rightflipper1
        Else 
            SoundFlipperUpAttackRight rightflipper1
            RandomSoundFlipperUpRight rightflipper1
        End If
    Else
        RightFlipper1.RotateToStart
        If rightflipper1.currentangle > rightflipper1.startAngle + 5 Then
            RandomSoundFlipperDownRight rightflipper1
        End If
        FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub


Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub LeftFlipper1_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper1, LFCount, parm									   
												   
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub


Sub RightFlipper1_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper1, RFCount, parm
End Sub
'*********** FLIPPERS PRIMITIVES *********************************

TimerFlipper.interval = 10
TimerFlipper.Enabled = 1
Sub TimerFlipper_Timer()
	batleft.objrotz = LeftFlipper.CurrentAngle + 1
	batleftshadow.objrotz = batleft.objrotz
    batleft1.objrotz = LeftFlipper1.CurrentAngle + 1
	batleftshadow.objrotz = batleft.objrotz
	batright.objrotz = RightFlipper.CurrentAngle + 1
	batrightshadow.objrotz = batright.objrotz
	batright1.objrotz = RightFlipper1.CurrentAngle + 1
	batrightshadow.objrotz = batright.objrotz
End sub


'*********
'Solenoids
'*********

SolCallBack(1) = "SolTrough"
SolCallBack(2) = "Auto_Plunger"
'SolCallBack(3) = "dtBankL.SolDropUp"
SolCallBack(3) = "ResetDrops"
SolCallBack(6) = "SolOutJail"
SolCallBack(7) = "bsLock.SolOut"
SolCallBack(8) = "CGate.Open ="
SolCallBack(12) = "bsUpper.SolOut"
SolCallBack(19) = "SolHotelDoor"
SolCallBack(24) = "SolKnocker"


Sub SolKnocker(Enabled)
        If enabled Then
                KnockerSolenoid 'Add knocker position object
        End If
End Sub

Sub SolTrough(Enabled)
    If Enabled Then
        bsTrough.ExitSol_On
        vpmTimer.PulseSw 15
    End If
End Sub

Sub Auto_Plunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub

Sub SolHotelDoor(Enabled)
    If Enabled Then
        fDoor.RotatetoEnd
        doorwall.IsDropped = 1
    Else
        fDoor.RotatetoStart
        doorwall.IsDropped = 0
    End If
End Sub

'****************
' Elvis animation
'****************

SolCallBack(29) = "ElvisLegs"
SolCallBack(30) = "ElvisArms"

Sub ElvisLegs(Enabled)
	Playsound "Elvis_legs"
    If Enabled Then
        fLegs.RotatetoStart
    Else
        fLegs.RotatetoEnd
    End If
End Sub

Sub ElvisArms(Enabled)
'	Playsound "Elvis_arms"
    If Enabled Then
        fArms.RotatetoStart
    Else
        fArms.RotatetoEnd
    End If
End Sub


' ************************************
' Switches, bumpers, lanes and targets
' ************************************
Sub Drain_Hit()
	if bsTrough.Balls >= 4 then ' delete excess (Debug)
		me.destroyball
		exit sub
	end If
	bsTrough.addball me : RandomSoundDrain Drain 
End Sub

Sub BallRelease_UnHit(): RandomSoundBallRelease ballrelease : End Sub

Sub HLock_Hit:playsound "fx_kicker_enter1":bsLock.AddBall Me:End Sub
'Sub HLock_Hit: bsLock.Addball me : SoundSaucerLock : End Sub
'Sub HLock_Unhit: bsLock.Addball me : SoundSaucerKick 1 : End Sub


Sub sw32_Hit(): bsUpper.Addball me : SoundSaucerLock : End Sub
Sub sw32_UnHit():  SoundSaucerKick 1, sw32 : End Sub




'Sub Drain_Hit:Me.destroyball:End Sub 'debug

Sub sw10_Hit:Controller.Switch(10) = 1:End Sub
Sub sw10_unHit:Controller.Switch(10) = 0:End Sub



Sub sw16_Hit : Controller.Switch(16)= 1 : BIPL=1 : End Sub
Sub sw16_unHit : Controller.Switch(16)= 0 : BIPL=0 :End Sub




' ************************************
' Jailhouse kicker
' ************************************

Dim KickerBall34

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180
    
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

Sub sw34_Hit
    set KickerBall34 = activeball
	Controller.Switch(34) = 1
    SoundSaucerLock
End Sub

Sub SolOutJail(Enable)
    If Enable then
		If Controller.Switch(34) <> 0 Then
			KickBall KickerBall34, 192, 29+RndNum(-1,1), 5, 15
			SoundSaucerKick 1, sw34
			Controller.Switch(34) = 0
		End If
	End If
End Sub

Sub sw34_UnHit : Controller.Switch(34)= 0 : End Sub

' ************************************
' Drop Targets
' ************************************

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

Dim DT1, DT2, DT3, DT4, DT5

Set DT1 = (new DropTarget)(sw17, sw17a, sw17p, 17, 0, false)
Set DT2 = (new DropTarget)(sw18, sw18a, sw18p, 18, 0, false)
Set DT3 = (new DropTarget)(sw19, sw19a, sw19p, 19, 0, false)
Set DT4 = (new DropTarget)(sw20, sw20a, sw20p, 20, 0, false)
Set DT5 = (new DropTarget)(sw21, sw21a, sw21p, 21, 0, false)


Dim DTArray
DTArray = Array(DT1, DT2, DT3, DT4, DT5)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 40 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 3 'VP units primitive raises above the up position on drops up
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
'                                DROP TARGETS FUNCTIONS
'******************************************************

'Add Timer name DTAnim to editor to handle drop target animations
DTAnim.interval = 10
DTAnim.enabled = True

Sub DTAnim_Timer()
	DoDTAnim
End Sub

Sub DTHit(switch)
        Dim i
        i = DTArrayID(switch)

        PlayTargetSound
        DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
        If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
                DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
        End If
        DoDTAnim
End Sub

Sub DTRaise(switch)
        Dim i
        i = DTArrayID(switch)

        DTArray(i).animate = -1
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
        For i = 0 to uBound(DTArray) 
                If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
        Next
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


'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
        dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
        rangle = (dtprim.rotz - 90) * 3.1416 / 180
        rangle2 = dtprim.rotz * 3.1416 / 180
        bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
        bangleafter = Atn2(aBall.vely,aball.velx)

        Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
        Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

        cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

        perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
        paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

        perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
        paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

        If perpvel > 0 and  perpvelafter <= 0 Then
                If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
                        DTCheckBrick = 3
                Else
                        DTCheckBrick = 1
                End If
        ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
                DTCheckBrick = 4
        Else 
                DTCheckBrick = 0
        End If
End Function


Sub DoDTAnim()
        Dim i
        For i=0 to Ubound(DTArray)
                DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
        Next
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
        dim transz, switchid
        Dim animtime, rangle

        switchid = switch

        rangle = prim.rotz * PI / 180

        DTAnimate = animate

        if animate = 0  Then
                primary.uservalue = 0
                DTAnimate = 0
                Exit Function
        Elseif primary.uservalue = 0 then 
                primary.uservalue = gametime
        end if

        animtime = gametime - primary.uservalue

        If (animate = 1 or animate = 4) and animtime < DTDropDelay Then
                primary.collidable = 0
        If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
                prim.rotx = DTMaxBend * cos(rangle)
                prim.roty = DTMaxBend * sin(rangle)
                DTAnimate = animate
                Exit Function
                elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
                primary.collidable = 0
                If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
                prim.rotx = DTMaxBend * cos(rangle)
                prim.roty = DTMaxBend * sin(rangle)
                animate = 2
                PlaySoundAt SoundFX(DTDropSound,DOFDropTargets),prim
        End If

        if animate = 2 Then
                transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
                if prim.transz > -DTDropUnits  Then
                        prim.transz = transz
                end if

                prim.rotx = DTMaxBend * cos(rangle)/2
                prim.roty = DTMaxBend * sin(rangle)/2

                if prim.transz <= -DTDropUnits Then 
                        prim.transz = -DTDropUnits
                        secondary.collidable = 0
                        controller.Switch(Switchid) = 1
                        primary.uservalue = 0
                        DTAnimate = 0
                        Exit Function
                Else
                        DTAnimate = 2
                        Exit Function
                end If 
        End If

        If animate = 3 and animtime < DTDropDelay Then
                primary.collidable = 0
                secondary.collidable = 1
                prim.rotx = DTMaxBend * cos(rangle)
                prim.roty = DTMaxBend * sin(rangle)
        elseif animate = 3 and animtime > DTDropDelay Then
                primary.collidable = 1
                secondary.collidable = 0
                prim.rotx = 0
                prim.roty = 0
                primary.uservalue = 0
                DTAnimate = 0
                Exit Function
        End If

        if animate = -1 Then
                transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1

                If prim.transz = -DTDropUnits Then
                        Dim BOT, b
                        BOT = GetBalls

                        For b = 0 to UBound(BOT)
                                If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and BOT(b).z < prim.z+DTDropUnits+25 Then
                                        BOT(b).velz = 20
                                End If
                        Next
                End If

                if prim.transz < 0 Then
                        prim.transz = transz
                elseif transz > 0 then
                        prim.transz = transz
                end if

                if prim.transz > DTDropUpUnits then 
                        prim.transz = DTDropUpUnits
                        DTAnimate = -2
                        prim.rotx = 0
                        prim.roty = 0
                        primary.uservalue = gametime
                end if
                primary.collidable = 0
                secondary.collidable = 1
                controller.Switch(Switchid) = 0

        End If

        if animate = -2 and animtime > DTRaiseDelay Then
                prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
                if prim.transz < 0 then
                        prim.transz = 0
                        primary.uservalue = 0
                        DTAnimate = 0

                        primary.collidable = 1
                        secondary.collidable = 0
                end If 
        End If
End Function


' Used for drop targets
Function Atn2(dy, dx)
 '        dim pi
 '        pi = 4*Atn(1)

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

' Used for drop targets
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

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function

' Used for drop targets
Function Distance(ax,ay,bx,by)
        Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function


Sub ResetDrops(enabled)
     if enabled then
          PlaySoundAt SoundFX(DTResetSound,DOFContactors), sw19p
          DTRaise 17
          DTRaise 18
          DTRaise 19
          DTRaise 20
		  DTRaise 21
     end if
End Sub


Sub sw17_Hit:DTHit 17:End Sub
Sub sw18_Hit:DTHit 18:End Sub
Sub sw19_Hit:DTHit 19:End Sub
Sub sw20_Hit:DTHit 20:End Sub
Sub sw21_Hit:DTHit 21:End Sub


Sub sw25_Spin:PlaySound "fx_spinner", 0, 1, -0.01:vpmTimer.PulseSw 25:End Sub
Sub sw26_Hit:Controller.Switch(26) = 1:End Sub
Sub sw26_unHit:Controller.Switch(26) = 0:End Sub
Sub sw27_Hit:Controller.Switch(27) = 1:End Sub
Sub sw27_unHit:Controller.Switch(27) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1:PlaySound "fx_metalrolling",0, 1, 0.15, 0.35:End Sub
Sub sw28_unHit:Controller.Switch(28) = 0:End Sub
Sub sw30_Hit:Controller.Switch(30) = 1:End Sub
Sub sw30_unHit:Controller.Switch(30) = 0:End Sub
Sub sw31_Hit:Controller.Switch(31) = 1:PlaySound "fx_metalrolling",0, 1, -0.15, 0.35:End Sub
Sub sw31_unHit:Controller.Switch(31) = 0:End Sub

Sub sw9_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 9
End Sub

Sub sw22_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 22
End Sub

Sub sw23_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 23
End Sub

Sub sw24_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 24
End Sub

Sub sw36_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 36
End Sub

Sub sw37_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 37
End Sub

Sub sw38_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 38
End Sub

Sub sw39_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 39
End Sub

Sub sw40_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 40
End Sub

Sub sw52_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 52
End Sub

Sub sw53_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 53
End Sub


Sub sw41_Hit:Controller.Switch(41) = 1:End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:End Sub
Sub sw42_Hit:Controller.Switch(42) = 1:End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub
Sub sw43_Hit:Controller.Switch(43) = 1:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_unHit:Controller.Switch(45) = 0:End Sub
Sub sw46_Hit:Controller.Switch(46) = 1:End Sub
Sub sw46_unHit:Controller.Switch(46) = 0:End Sub
Sub sw47_Hit:Controller.Switch(47) = 1:End Sub
Sub sw47_unHit:Controller.Switch(47) = 0:End Sub
Sub sw48_Hit:Controller.Switch(48) = 1:End Sub
Sub sw48_unHit:Controller.Switch(48) = 0:End Sub


'Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 49:RandomSoundBumperMiddle bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 50:RandomSoundBumperTop bumper2:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 51:RandomSoundBumperBottom bumper3:End Sub

Sub sw57_Hit::Controller.Switch(57) = 1:End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw58_Hit:Controller.Switch(58) = 1:End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub sw60_Hit:Controller.Switch(60) = 1:End Sub
Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub
Sub sw61_Hit:Controller.Switch(61) = 1:End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub


'Ramps triggers
Sub TriggerSLide1_Hit():PlaySound "metal_slide1": End Sub
Sub TriggerSlide2_Hit():PlaySound "metal_slide1": End Sub

Sub TriggerLRampStop_Hit()
	StopSound "fx_metalrolling"
	PlaySound "WireRamp_Stop1"
End Sub

Sub TriggerRRampStop_Hit()
	StopSound "fx_metalrolling"
	PlaySound "WireRamp_Stop2"
End Sub

Sub LRhit_Hit():PlaySound "WireRamp_Hit": End Sub
Sub RRhit_Hit():PlaySound "WireRamp_Hit": End Sub


'*********** SLINGSHOTS *********************************

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 59
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select

    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 62
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select

    RStep = RStep + 1
End Sub


Sub sw63_UnHit:PlaySound"fx_balldrop1",0,1,0.1:End Sub
sub doorwall_hit() : vpmtimer.PulseSw 63 : end Sub 


Sub sw64_Hit
    vpmTimer.PulseSw 64
    str = INT(ABS(cor.ballvely(activeball.id)))*4
    debug.print str
    DogDir = 10 'upwards
    HoundDogTimer.Enabled = 1
    TargetBouncer Activeball, 1

End Sub

' Animate Hound Dog
Dim str 'strength of the hit
Dim DogStep, DogDir
DogStep = 0
DogDir = 0

Sub HoundDogTimer_Timer()
    DogStep = DogStep + DogDir
    If DogStep<0 Then DogStep=0
    HoundDog.TransZ = DogStep
    If DogStep> 100 Then DogDir = -10
    If DogStep> str Then DogDir = -10
    If DogStep <5 Then HoundDogTimer.Enabled = 0
End Sub


Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200), FlashRepeat(200)


Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub
'
Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
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

InitPolarity

'*******************************************
' Early 90's and after

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
																			
														   
												

			x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 ' disabled
			x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
			x.enabled = True
			x.TimeDelay = 60
			x.DebugOn = False ' prints some info in debugger

			x.AddPt "Polarity", 0, 0, 0
			x.AddPt "Polarity", 1, 0.05, -5.5
			x.AddPt "Polarity", 2, 0.40, -5.5
			x.AddPt "Polarity", 3, 0.60, -5.0
			x.AddPt "Polarity", 4, 0.65, -4.5
			x.AddPt "Polarity", 5, 0.70, -4.0
			x.AddPt "Polarity", 6, 0.75, -3.5
			x.AddPt "Polarity", 7, 0.80, -3.0
			x.AddPt "Polarity", 8, 0.85, -2.5
			x.AddPt "Polarity", 9, 0.90, -2.0
			x.AddPt "Polarity", 10,0.95, -1.5
			x.AddPt "Polarity", 11,1.00, -1.0
			x.AddPt "Polarity", 12,1.05, -0.5
			x.AddPt "Polarity", 13,1.10, 0
			x.AddPt "Polarity", 14,1.30, 0

			x.AddPt "Velocity", 0, 0.000, 1
			x.AddPt "Velocity", 1, 0.160, 1.06
			x.AddPt "Velocity", 2, 0.410, 1.05
			x.AddPt "Velocity", 3, 0.530, 1 ' 0.982
			x.AddPt "Velocity", 4, 0.702, 0.968
			x.AddPt "Velocity", 5, 0.950, 0.968
			x.AddPt "Velocity", 6, 1.030, 0.945

        Next

		' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
		LF.SetObjects "LF", LeftFlipper, TriggerLF 
		RF.SetObjects "RF", RightFlipper, TriggerRF 

End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers. 
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger) 
'	Otherwise it should function exactly the same as before

Class FlipperPolarity 
	Public DebugOn, Enabled
	Private FlipAt        ' Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        ' delay before trigger turns off and polarity is disabled
	private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	private Name

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Sub SetObjects(aName, aFlipper, aTrigger) 
									
					
						   
				   
				   

		if typename(aName) <> "String" then msgbox "FlipperPolarity: .SetObjects error: first argument must be a string (and name of Object). Found:" & typename(aName) end if
		if typename(aFlipper) <> "Flipper" then msgbox "FlipperPolarity: .SetObjects error: second argument must be a flipper. Found:" & typename(aFlipper) end if
		if typename(aTrigger) <> "Trigger" then msgbox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & typename(aTrigger) end if
		if aFlipper.EndAngle > aFlipper.StartAngle then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper : FlipperStart = aFlipper.x 
		FlipperEnd = Flipper.Length * sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y

		dim str : str = "sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'" ' automatically create hit / unhit events if uncommented
		ExecuteGlobal(str)
		str = "sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)	

	End Sub

	Public Property Let EndPoint(aInput) :  : End Property ' Legacy: just no op 

	Public Sub AddPt(aChooseArray, aIDX, aX, aY) ' Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		   
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

	Public Property Get Pos ' returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() ' save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        ' Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			' y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			' Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                ' find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then ' no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                ' find safety coefficient 'ycoef' data
			End If

			' Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			' Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			if DebugOn then debug.print "PolarityCorrect" & " " & Name & " @ " & gametime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class
'******************************************************
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************


Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


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
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
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


'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************


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
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
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

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

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
		Dim b, BOT
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


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR




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
	end if
end sub

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
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



'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.
'
' NOTE: The below timer is for flashing the inserts as a demonstration of Lampz. Should be replaced by actual lamp states.
'       In other words, delete this sub (InsertFlicker_timer) and associated timer if you are going to use Lampz with a ROM.
'dim flickerX, FlickerState : FlickerState = 0

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
'Dim ModLampz : Set ModLampz = new NoFader ' NoFader just updates as calls come in																											 
Dim ModLampz : Set ModLampz = new NoFader2 ' NoFader2 requires .Update																								 
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
	Lampz.Update2	 ' update
	ModLampz.Update ' update (object updates only, pinmame itself sends the fading info)
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub


Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (max level / full MS fading time). The Modulate property must be set to 1 / max level if lamp is modulated.
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/75 : Lampz.FadeSpeedDown(x) = 1/85 : next
	'Flasher related
	for x = 117 to 124 : Lampz.FadeSpeedUp(110) = 255/30 : Lampz.FadeSpeedDown(110) = 255/90 :  Lampz.Modulate(110) = 1/255 :next

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	
	Lampz.MassAssign(1)= L1
	Lampz.MassAssign(1)= L1a
	Lampz.Callback(1) = "DisableLighting p1on, 100,"

	Lampz.MassAssign(2)= L2
	Lampz.MassAssign(2)= L2a
	Lampz.MassAssign(2)= F2
	Lampz.MassAssign(2)= F2a
	Lampz.Callback(2) = "DisableLighting p2on, 100,"
	
	Lampz.MassAssign(3)= L3
	Lampz.MassAssign(3)= L3a
	Lampz.MassAssign(3)= F3
	Lampz.Callback(3) = "DisableLighting p3on, 100,"

	
	Lampz.MassAssign(4)= L4
	Lampz.MassAssign(4)= L4a
	Lampz.MassAssign(4)= F4
	Lampz.Callback(4) = "DisableLighting p4on, 100,"

	Lampz.MassAssign(5)= L5
	Lampz.MassAssign(5)= L5a
	Lampz.MassAssign(5)= F5
	Lampz.Callback(5) = "DisableLighting p5on, 100,"

	Lampz.MassAssign(6)= L6
	Lampz.MassAssign(6)= L6a
	Lampz.MassAssign(6)= F6
	Lampz.Callback(6) = "DisableLighting p6on, 100,"

	Lampz.MassAssign(7)= L7
	Lampz.MassAssign(7)= L7a
	Lampz.MassAssign(7)= F7
	Lampz.Callback(7) = "DisableLighting p7on, 100,"

	Lampz.MassAssign(8)= L8
	Lampz.MassAssign(8)= L8a
	Lampz.MassAssign(8)= F8
	Lampz.Callback(8) = "DisableLighting p8on, 100,"

	Lampz.MassAssign(9)= L9
	Lampz.MassAssign(9)= L9a
	Lampz.Callback(9) = "DisableLighting p9on, 100,"

	Lampz.MassAssign(10)= L10
	Lampz.MassAssign(10)= L10a
	Lampz.Callback(10) = "DisableLighting p10on, 100,"

	Lampz.MassAssign(11)= L11
	Lampz.MassAssign(11)= L11a
	Lampz.Callback(11) = "DisableLighting p11on, 100,"

	Lampz.MassAssign(12)= L12
	Lampz.MassAssign(12)= L12a
	Lampz.Callback(12) = "DisableLighting p12on, 100,"

	Lampz.MassAssign(13)= L13
	Lampz.MassAssign(13)= L13a
	Lampz.Callback(13) = "DisableLighting p13on, 100,"

	Lampz.MassAssign(14)= L14
	Lampz.MassAssign(14)= L14a
	Lampz.Callback(14) = "DisableLighting p14on, 100,"

	Lampz.MassAssign(15)= L15
	Lampz.MassAssign(15)= L15a
	Lampz.Callback(15) = "DisableLighting p15on, 100,"

	Lampz.MassAssign(16)= L16
	Lampz.MassAssign(16)= L16a
	Lampz.Callback(16) = "DisableLighting p16on, 100,"

	Lampz.MassAssign(17)= L17
	Lampz.MassAssign(17)= L17a
	Lampz.Callback(17) = "DisableLighting p17on, 100,"

	Lampz.MassAssign(18)= L18
	Lampz.MassAssign(18)= L18a
	Lampz.Callback(18) = "DisableLighting p18on, 100,"

	Lampz.MassAssign(19)= L19
	Lampz.MassAssign(19)= L19a
	Lampz.Callback(19) = "DisableLighting p19on, 100,"

	Lampz.MassAssign(20)= L20
	Lampz.MassAssign(20)= L20a
	Lampz.Callback(20) = "DisableLighting p20on, 100,"

	Lampz.MassAssign(21)= L21
	Lampz.MassAssign(21)= L21a
	Lampz.Callback(21) = "DisableLighting p21on, 100,"

	Lampz.MassAssign(22)= L22
	Lampz.MassAssign(22)= L22a
	Lampz.MassAssign(22)= F22
	Lampz.Callback(22) = "DisableLighting p22on, 100,"

	Lampz.MassAssign(23)= L23
	Lampz.MassAssign(23)= L23a
	Lampz.Callback(23) = "DisableLighting p23on, 100,"

	Lampz.MassAssign(24)= L24
	Lampz.MassAssign(24)= L24a
	Lampz.Callback(24) = "DisableLighting p24on, 100,"

	Lampz.MassAssign(25)= L25
	Lampz.MassAssign(25)= L25a
	Lampz.Callback(25) = "DisableLighting p25on, 100,"

	Lampz.MassAssign(26)= L26
	Lampz.MassAssign(26)= L26a
	Lampz.Callback(26) = "DisableLighting p26on, 100,"

	Lampz.MassAssign(27)= L27
	Lampz.MassAssign(27)= L27a
	Lampz.Callback(27) = "DisableLighting p27on, 100,"

	Lampz.MassAssign(28)= L28
	Lampz.MassAssign(28)= L28a
	Lampz.Callback(28) = "DisableLighting p28on, 100,"

	Lampz.MassAssign(29)= L29
	Lampz.MassAssign(29)= L29a
	Lampz.Callback(29) = "DisableLighting p29on, 100,"

	Lampz.MassAssign(30)= L30
	Lampz.MassAssign(30)= L30a
	Lampz.MassAssign(30)= F30
	Lampz.Callback(30) = "DisableLighting p30on, 100,"

	Lampz.MassAssign(31)= L31
	Lampz.MassAssign(31)= L31a
	Lampz.Callback(31) = "DisableLighting p31on, 100,"


	Lampz.MassAssign(32)= L32
	Lampz.MassAssign(32)= L32a
	Lampz.Callback(32) = "DisableLighting p32on, 100,"

	Lampz.MassAssign(33)= L33
	Lampz.MassAssign(33)= L33a
	Lampz.MassAssign(33)= F33
	Lampz.Callback(33) = "DisableLighting p33on, 100,"

	Lampz.MassAssign(34)= L34
	Lampz.MassAssign(34)= L34a
	Lampz.MassAssign(34)= F34
	Lampz.Callback(34) = "DisableLighting p34on, 100,"

	Lampz.MassAssign(35)= L35
	Lampz.MassAssign(35)= L35a
	Lampz.MassAssign(35)= F35
	Lampz.Callback(35) = "DisableLighting p35on, 100,"

	Lampz.MassAssign(36)= L36
	Lampz.MassAssign(36)= L36a
	Lampz.MassAssign(36)= F36
	Lampz.Callback(36) = "DisableLighting p36on, 100,"

	Lampz.MassAssign(37)= L37
	Lampz.MassAssign(37)= L37a
	Lampz.MassAssign(37)= F37
	Lampz.Callback(37) = "DisableLighting p37on, 100,"

	Lampz.MassAssign(38)= L38
	Lampz.MassAssign(38)= L38a
	Lampz.MassAssign(38)= F38
	Lampz.Callback(38) = "DisableLighting p38on, 100,"

	Lampz.MassAssign(39)= L39
	Lampz.MassAssign(39)= L39a
	Lampz.Callback(39) = "DisableLighting p39on, 100,"

	Lampz.MassAssign(40)= L40
	Lampz.MassAssign(40)= L40a
	Lampz.Callback(40) = "DisableLighting p40on, 100,"

	Lampz.MassAssign(41)= L41
	Lampz.MassAssign(41)= L41a
	Lampz.MassAssign(41)= F41
	Lampz.Callback(41) = "DisableLighting p41on, 100,"

	Lampz.MassAssign(42)= L42
	Lampz.MassAssign(42)= L42a
	Lampz.Callback(42) = "DisableLighting p42on, 100,"

	Lampz.MassAssign(43)= L43
	Lampz.MassAssign(43)= L43a
	Lampz.Callback(43) = "DisableLighting p43on, 100,"

	Lampz.MassAssign(44)= L44
	Lampz.MassAssign(44)= L44a
	Lampz.Callback(44) = "DisableLighting p44on, 100,"

	Lampz.MassAssign(45)= L45
	Lampz.MassAssign(45)= L45a
	Lampz.MassAssign(45)= F45
	Lampz.Callback(45) = "DisableLighting p45on, 100,"

	Lampz.MassAssign(46)= L46
	Lampz.MassAssign(46)= L46a
	Lampz.MassAssign(46)= F46
	Lampz.Callback(46) = "DisableLighting p46on, 100,"

	Lampz.MassAssign(47)= L47
	Lampz.MassAssign(47)= L47a
	Lampz.MassAssign(47)= F47
	Lampz.Callback(47) = "DisableLighting p47on, 100,"

	Lampz.MassAssign(48)= L48
	Lampz.MassAssign(48)= L48a
	Lampz.MassAssign(48)= F48
	Lampz.Callback(48) = "DisableLighting p48on, 100,"

	Lampz.MassAssign(49)= L49
	Lampz.MassAssign(49)= L49a
	Lampz.MassAssign(49)= F49
	Lampz.Callback(49) = "DisableLighting p49on, 100,"

	Lampz.MassAssign(50)= L50
	Lampz.MassAssign(50)= L50a
	Lampz.Callback(50) = "DisableLighting p50on, 100,"

	Lampz.MassAssign(51)= L51
	Lampz.MassAssign(51)= L51a
	Lampz.Callback(51) = "DisableLighting p51on, 100,"

	Lampz.MassAssign(52)= L52
	Lampz.MassAssign(52)= L52a
	Lampz.Callback(52) = "DisableLighting p52on, 100,"

	Lampz.MassAssign(53)= L53
	Lampz.MassAssign(53)= L53a
	Lampz.Callback(53) = "DisableLighting p53on, 100,"

	Lampz.MassAssign(54)= L54
	Lampz.MassAssign(54)= L54a
	Lampz.MassAssign(54)= F54
	Lampz.Callback(54) = "DisableLighting p54on, 100,"

	Lampz.MassAssign(55)= L55
	Lampz.MassAssign(55)= L55a
	Lampz.Callback(55) = "DisableLighting p55on, 100,"

	Lampz.MassAssign(56)= L56
	Lampz.MassAssign(56)= L56a
	Lampz.Callback(56) = "DisableLighting p56on, 100,"

	Lampz.MassAssign(57)= L57
	Lampz.MassAssign(57)= L57a
	Lampz.MassAssign(57)= L57b
	Lampz.MassAssign(57)= L57c
	Lampz.Callback(57) = "DisableLighting p57on, 100,"

	Lampz.MassAssign(58)= L58
	Lampz.MassAssign(58)= L58a
	Lampz.MassAssign(58)= L58b
	Lampz.MassAssign(58)= L58c
	Lampz.Callback(58) = "DisableLighting p58on, 100,"

	Lampz.MassAssign(59)= L59
	Lampz.MassAssign(59)= L59a
	Lampz.MassAssign(59)= L59b
	Lampz.MassAssign(59)= L59c
	Lampz.Callback(59) = "DisableLighting p59on, 100,"

	Lampz.MassAssign(60)= L60
	Lampz.MassAssign(60)= L60a
	Lampz.Callback(60) = "DisableLighting p60on, 100,"

	Lampz.MassAssign(61)= L61
	Lampz.MassAssign(61)= L61a
	Lampz.MassAssign(61)= L61b
	Lampz.Callback(61) = "DisableLighting p61on, 100,"

	Lampz.MassAssign(62)= L62
	Lampz.Callback(62) = "DisableLighting p62on, 100,"

	Lampz.MassAssign(63)= L63
	Lampz.MassAssign(63)= L63a
	Lampz.MassAssign(63)= F63
	Lampz.Callback(63) = "DisableLighting p63on, 100,"

	Lampz.MassAssign(64)= L64
	Lampz.MassAssign(64)= L64a
	Lampz.MassAssign(64)= F64
	Lampz.Callback(64) = "DisableLighting p64on, 100,"

	Lampz.MassAssign(65)= L65
	Lampz.MassAssign(65)= L65a
	Lampz.MassAssign(65)= L65b
	Lampz.MassAssign(65)= L65c
	Lampz.MassAssign(65)= L65d
	Lampz.MassAssign(65)= L65e
	Lampz.MassAssign(65)= L65f
	Lampz.Callback(65) = "DisableLighting p65, .05,"
	Lampz.Callback(65) = "DisableLighting p65a, 50,"

	Lampz.MassAssign(66)= L66
	Lampz.MassAssign(66)= L66a
	Lampz.MassAssign(66)= L66b
	Lampz.MassAssign(66)= L66c
	Lampz.MassAssign(66)= L66d
	Lampz.Callback(66) = "DisableLighting p66, .05,"
	Lampz.Callback(66) = "DisableLighting p66a, 50,"

	Lampz.MassAssign(67)= L67
	Lampz.MassAssign(67)= L67a
	Lampz.MassAssign(67)= L67b
	Lampz.MassAssign(67)= L67c
	Lampz.MassAssign(67)= L67d
	Lampz.Callback(67) = "DisableLighting p67, .05,"
	Lampz.Callback(67) = "DisableLighting p67a, 50,"

	Lampz.MassAssign(68)= L68
	Lampz.MassAssign(68)= L68a
	Lampz.MassAssign(68)= L68b
	Lampz.MassAssign(68)= L68c
	Lampz.MassAssign(68)= L68d
	Lampz.Callback(68) = "DisableLighting p68, .05,"
	Lampz.Callback(68) = "DisableLighting p68a, 50,"

	Lampz.MassAssign(69)= L69
	Lampz.MassAssign(69)= L69a
	Lampz.MassAssign(69)= L69b
	Lampz.MassAssign(69)= L69c
	Lampz.MassAssign(69)= L69d
	Lampz.MassAssign(69)= L69e
	Lampz.Callback(69) = "DisableLighting p69, .05,"
	Lampz.Callback(69) = "DisableLighting p69a, 50,"

	Lampz.MassAssign(70)= L70
	Lampz.MassAssign(70)= L70a
'	Lampz.MassAssign(70)= L70b
	Lampz.MassAssign(70)= l70c
	Lampz.Callback(70) = "DisableLighting p70, .05,"
	Lampz.Callback(70) = "DisableLighting p70a, 50,"

	Lampz.MassAssign(71)= L71
	Lampz.MassAssign(71)= L71a
'	Lampz.MassAssign(71)= L71b
	Lampz.MassAssign(71)= L71c
	Lampz.Callback(71) = "DisableLighting p71, .05,"
	Lampz.Callback(71) = "DisableLighting p71a, 50,"

	Lampz.MassAssign(72)= L72
	Lampz.MassAssign(72)= L72a
'	Lampz.MassAssign(72)= L72b
	Lampz.MassAssign(72)= L72c
	Lampz.MassAssign(72)= L72d
	Lampz.MassAssign(72)= L72e
	Lampz.Callback(72) = "DisableLighting p72, .05,"
	Lampz.Callback(72) = "DisableLighting p72a, 50,"

	Lampz.MassAssign(73)= L73
	Lampz.MassAssign(73)= L73a
	Lampz.MassAssign(73)= L73b
	Lampz.Callback(73) = "DisableLighting p73on, 100,"
	
	Lampz.MassAssign(74)= L74		
	Lampz.MassAssign(74)= L74a
	Lampz.Callback(74) = "DisableLighting p74on, 100,"
	
	Lampz.MassAssign(75)= L75
	Lampz.MassAssign(75)= L75a
	Lampz.Callback(75) = "DisableLighting p75on, 100,"

	Lampz.MassAssign(76)= L76
	Lampz.MassAssign(76)= L76a
	Lampz.Callback(76) = "DisableLighting p76on, 100,"

	Lampz.MassAssign(77)= L77
	Lampz.MassAssign(77)= L77a
	Lampz.Callback(77) = "DisableLighting p77on, 100,"

	Lampz.MassAssign(78)= L78
	Lampz.MassAssign(78)= L78a
	Lampz.Callback(78) = "DisableLighting p78on, 100,"
	if not UseVPMModSol then 
	
		Lampz.MassAssign(123)= F123
		Lampz.MassAssign(123)= F123a
		Lampz.MassAssign(123)= F123b
		Lampz.MassAssign(123)= F123c
		Lampz.MassAssign(123)= F123d
		Lampz.MassAssign(123)= F123e
		Lampz.MassAssign(131)= F131
		Lampz.MassAssign(131)= F131a

	else 
		ModLampz.Callback(20) = "Flash20Callback" ' top left, top right flashers
		ModLampz.Callback(21) = "Flash21Callback" ' top center flasher
		ModLampz.Callback(22) = "Flash22Callback" ' left middle flasher

		ModLampz.MassAssign(23)= F123 ' right pf flasher
		ModLampz.MassAssign(23)= F123a
		ModLampz.MassAssign(23)= F123b
		ModLampz.MassAssign(23)= F123c
		ModLampz.MassAssign(23)= F123d
		ModLampz.MassAssign(23)= F123e

		ModLampz.MassAssign(31)= F131 ' center pf flasher
		ModLampz.MassAssign(31)= F131a

		ModLampz.Callback(32) = "Flash32Callback" ' left, right slingshot flashers
		
		ModLampz.Init

	end if
	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


'====================
'Class jungle nf
'====================

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
'Version 0.14 - Updated to support modulated signals - Niwak

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


'******************************************************
'*****   FLUPPER DOMES 
'******************************************************
' Based on FlupperDoms2.2

' What you need in your table to use these flashers:
' Open this table and your table both in VPX
' Export all the materials domebasemat, Flashermaterial0 - 20 and import them in your table
' Export all textures (images) starting with the name "dome" and "ronddome" and import them into your table with the same names
' Export all textures (images) starting with the name "flasherbloom" and import them into your table with the same names
' Copy a set of 4 objects flasherbase, flasherlit, flasherlight and flasherflash from layer 7 to your table
' If you duplicate the four objects for a new flasher dome, be sure that they all end with the same number (in the 0-20 range)
' Copy the flasherbloom flashers from layer 10 to your table. you will need to make one per flasher dome that you plan to make
' Select the correct flasherbloom texture for each flasherbloom flasher, per flasher dome
' Copy the script below 

' Place your flasher base primitive where you want the flasher located on your Table
' Then run InitFlasher in the script with the number of your flasher objects and the color of the flasher.  This will align the flasher object, light object, and 
' flasher lit primitive.  It will also assign the appropriate flasher bloom images to the flasher bloom object.
'
' Example: InitFlasher 1, "green"
'
' Color Options: "blue", "green", "red", "purple", "yellow", "white", and "orange"

' You can use the RotateFlasher call to align the Rotz/ObjRotz of the flasher primitives with "handles".  Don't set those values in the editor,
' call the RotateFlasher sub instead (this call will likely crash VP if it's call for the flasher primitives without "handles")
'
' Example: RotateFlasher 1, 180 		'where 1 is the flasher number and 180 is the angle of Z rotation

' For flashing the flasher use in the script: "ObjLevel(1) = 1 : FlasherFlash1_Timer"
' This should also work for flashers with variable flash levels from the rom, just use ObjLevel(1) = xx from the rom (in the range 0-1)
'
' Notes (please read!!):
' - Setting TestFlashers = 1 (below in the ScriptsDirectory) will allow you to see how the flasher objects are aligned (need the targetflasher image imported to your table)
' - The rotation of the primitives with "handles" is done with a script command, not on the primitive itself (see RotateFlasher below)
' - Color of the objects are set in the script, not on the primitive itself
' - Screws are optional to copy and position manually
' - If your table is not named "Table1" then change the name below in the script
' - Every flasher uses its own material (Flashermaterialxx), do not use it for anything else
' - Lighting > Bloom Strength affects how the flashers look, do not set it too high
' - Change RotY and RotX of flasherbase only when having a flasher something other then parallel to the playfield
' - Leave RotX of the flasherflash object to -45; this makes sure that the flash effect is visible in FS and DT
' - If you want to resize a flasher, be sure to resize flasherbase, flasherlit and flasherflash with the same percentage
' - If you think that the flasher effects are too bright, change flasherlightintensity and/or flasherflareintensity below

' Some more notes for users of the v1 flashers and/or JP's fading lights routines:
' - Delete all textures/primitives/script/materials in your table from the v1 flashers and scripts before you start; they don't mix well with v2
' - Remove flupperflash(m) routines if you have them; they do not work with this new script
' - Do not try to mix this v2 script with the JP fading light routine (that is making it too complicated), just use the example script below

' example script for rom based tables (non modulated):

' SolCallback(25)="FlashRed"
'
' Sub FlashRed(flstate)
'	If Flstate Then
'		ObjTargetLevel(1) = 1
'	Else
'		ObjTargetLevel(1) = 0
'	End If
'   FlasherFlash1_Timer
' End Sub

' example script for rom based tables (modulated):

' SolModCallback(25)="FlashRed"
'
' Sub FlashRed(level)
'	ObjTargetLevel(1) = level/255 : FlasherFlash1_Timer
' End Sub


' Sub FlashSol122(Enabled)
'	If Enabled Then		
'		ObjTargetLevel(1) = 1
'	Else
'		ObjTargetLevel(1) = 0
'	End If
'	FlasherFlash1_Timer
'	Sound_Flash_Relay enabled, Flasherbase1
' End Sub


'*********
' Flashers
'*********
if not UseVPMModSol then 
	SolCallBack(20) = "FlashSol120" 'top left, top right flashers
	SolCallback(21) = "FlashSol121" 'top center flasher
	SolCallBack(22) = "FlashSol122" 'left middle flasher
	SolCallBack(23) = "Lampz.SetLamp 123," 'right pf flasher
	'SolCallBack(23) = "SetLamp 123," 'right pf flasher
	SolCallBack(31) = "Lampz.SetLamp 131," 'center pf flasher
	'SolCallBack(31) = "SetLamp 131," 'center pf flasher
	SolCallBack(32) = "FlashSol132" 'left, right slingshot flashers

else 
	SolModCallBack(20) = "ModLampz.SetModLamp 20," 'top left, top right flashers
	SolModCallBack(21) = "ModLampz.SetModLamp 21," 'top center flasher
	SolModCallBack(22) = "ModLampz.SetModLamp 22," 'left middle flasher
	SolModCallBack(23) = "ModLampz.SetModLamp 23," ' right pf flasher

	SolModCallBack(31) = "ModLampz.SetModLamp 31," 'center pf flasher
	SolModCallBack(32) = "ModLampz.SetModLamp 32," 'left, right slingshot flashers

end if


 Sub FlashSol132(Enabled)
	If Enabled Then
		ObjTargetLevel(1) = 1
		ObjTargetLevel(2) = 1
	Else
		ObjTargetLevel(1) = 0
		ObjTargetLevel(2) = 0
	End If
	FlasherFlash1_Timer
	FlasherFlash2_Timer
'	Sound_Flash_Relay enabled, Flasherbase2
 End Sub

 Sub FlashSol122(Enabled)
	If Enabled Then		
		ObjTargetLevel(3) = 1
	Else
		ObjTargetLevel(3) = 0
	End If
	FlasherFlash3_Timer
'	Sound_Flash_Relay enabled, Flasherbase3
 End Sub

 Sub FlashSol120(Enabled)
	If Enabled Then
		ObjTargetLevel(4) = 1
		ObjTargetLevel(6) = 1
	Else
		ObjTargetLevel(4) = 0
		ObjTargetLevel(6) = 0
	End If
	FlasherFlash4_Timer
	FlasherFlash6_Timer
'	Sound_Flash_Relay enabled, Flasherbase4
 End Sub

 Sub FlashSol121(Enabled)
	If Enabled Then
		ObjTargetLevel(5) = 1
	Else
		ObjTargetLevel(5) = 0
	End If
	FlasherFlash5_Timer
'	Sound_Flash_Relay enabled, Flasherbase5
 End Sub


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.2		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.3		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.1		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.3		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "red"
InitFlasher 2, "red"
InitFlasher 3, "red"
InitFlasher 4, "yellow"
InitFlasher 5, "white"
InitFlasher 6, "yellow"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 


Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 40
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

'	'rothbauerw
'	'Adjust the position of the flasher object to align with the flasher base.
'	'Comment out these lines if you want to manually adjust the flasher object
'	If objbase(nr).roty > 135 then
'		objflasher(nr).y = objbase(nr).y + 50
'		objflasher(nr).height = objbase(nr).z + 20
'	Else
'		objflasher(nr).y = objbase(nr).y + 20
'		objflasher(nr).height = objbase(nr).z + 50
'	End If
'	objflasher(nr).x = objbase(nr).x
'
'	'rothbauerw
'	'Adjust the position of the light object to align with the flasher base.
'	'Comment out these lines if you want to manually adjust the flasher object
'	objlight(nr).x = objbase(nr).x
'	objlight(nr).y = objbase(nr).y
'	objlight(nr).bulbhaloheight = objbase(nr).z -10

	'rothbauerw
	'Assign the appropriate bloom image basked on the location of the flasher base
	'Comment out these lines if you want to manually assign the bloom images
	dim xthird, ythird
'	xthird = tablewidth/3
'	ythird = tableheight/3

	If objbase(nr).x >= xthird and objbase(nr).x <= xthird*2 then
		objbloom(nr).imageA = "flasherbloomCenter"
		objbloom(nr).imageB = "flasherbloomCenter"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperLeft"
		objbloom(nr).imageB = "flasherbloomUpperLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperRight"
		objbloom(nr).imageB = "flasherbloomUpperRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterLeft"
		objbloom(nr).imageB = "flasherbloomCenterLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterRight"
		objbloom(nr).imageB = "flasherbloomCenterRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerLeft"
		objbloom(nr).imageB = "flasherbloomLowerLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerRight"
		objbloom(nr).imageB = "flasherbloomLowerRight"
	end if

	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr) ' not used if UseVPMModSol = true
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	if round(ObjTargetLevel(nr),1) > round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) + 0.3
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif round(ObjTargetLevel(nr),1) < round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) * 0.85 - 0.01
		if ObjLevel(nr) < 0 then ObjLevel(nr) = 0
	Else
		ObjLevel(nr) = round(ObjTargetLevel(nr),1)
		objflasher(nr).TimerEnabled = False
	end if
	'ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub


Sub UpdateCaps(aNr, aValue) ' nf UseVPMModSol dynamic fading version of 'FlashFlasher' (PWM Update). You can see most of the ramp up has been commented out in favor of Pinmame's curves
	if aValue > 0 then 
		objflasher(aNr).visible = 1 : objbloom(aNr).visible = 1 : objlit(aNr).visible = 1
	else 
		objflasher(aNr).visible = 1 : objbloom(aNr).visible = 1 : objlit(aNr).visible = 1
	end if
	objflasher(aNr).opacity = 1000 *  FlasherFlareIntensity * aValue' * ObjLevel(nr)^2.5
	objbloom(aNr).opacity = 100 *  FlasherBloomIntensity * aValue' * ObjLevel(nr)^2.5
	objlight(aNr).IntensityScale = 0.5 * FlasherLightIntensity * aValue' * ObjLevel(nr)^3
	objbase(aNr).BlendDisableLighting =  FlasherOffBrightness + 10 * aValue '* ObjLevel(nr)^3	
	objlit(aNr).BlendDisableLighting = 10 * aValue '* ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & aNr, 0,0,0,0,0,0,Round(aValue,1),RGB(255,255,255),0,0,False,True,0,0,0,0 
End Sub


Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub ' fallback if UseVPMModSol=False
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 



sub Flash20Callback(aValue) ' 4 6 (far corner backwall caps)
	UpdateCaps 4, aValue
	UpdateCaps 6, aValue
end Sub

sub Flash21Callback(aValue) ' 5 
	UpdateCaps 5, aValue
end Sub

sub Flash22Callback(aValue) ' 3
	UpdateCaps 3, aValue
end Sub

sub Flash32Callback(aValue) ' 1 2 sling caps
	UpdateCaps 1, aValue
	UpdateCaps 2, aValue

'	' legacy behavior
'	If aValue > 0 Then
'		ObjTargetLevel(1) = 1
'		ObjTargetLevel(2) = 1
'	Else
'		ObjTargetLevel(1) = 0
'		ObjTargetLevel(2) = 0
'	End If
'
'	FlashFlasher(2) ' debug (legacy)

end Sub


'******************************************************
'******  END FLUPPER DOMES
'******************************************************

Sub Frametimer_Timer()
	Flasherflash3a.opacity = Flasherflash3.opacity
	Flasherflash3b.opacity = Flasherflash3.opacity*0.2
	Flasherflash3c.opacity = Flasherflash3.opacity*0.8
	Flasherflash2a.opacity = Flasherflash2.opacity
	Flasherflash2b.opacity = Flasherflash2.opacity*0.1
	Flasherflash1a.opacity = Flasherflash1.opacity
	Flasherflash1b.opacity = Flasherflash1.opacity*0.4
	Flasherflash4a.opacity = Flasherflash4.opacity*3
	Flasherflash4b.opacity = Flasherflash4.opacity*0.4
	Flasherflash4c.opacity = Flasherflash4.opacity*0.7
	Flasherflash5a.opacity = Flasherflash5.opacity*5
	Flasherflash6a.opacity = Flasherflash6.opacity*1.5
	Flasherflash6b.opacity = Flasherflash6.opacity*0.4
	Flasherflash6c.opacity = Flasherflash6.opacity*0.7


	Flasherflash3a.visible = Flasherflash3.visible
	Flasherflash3b.visible = Flasherflash3.visible
	Flasherflash3c.visible = Flasherflash3.visible
	Flasherflash2a.visible = Flasherflash2.visible
	Flasherflash2b.visible = Flasherflash2.visible
	Flasherflash1a.visible = Flasherflash1.visible
	Flasherflash1b.visible = Flasherflash1.visible
	Flasherflash4a.visible = Flasherflash4.visible
	Flasherflash4b.visible = Flasherflash4.visible
	Flasherflash4c.visible = Flasherflash4.visible
	Flasherflash5a.visible = Flasherflash5.visible
	Flasherflash6a.visible = Flasherflash6.visible
	Flasherflash6b.visible = Flasherflash6.visible
	Flasherflash6c.visible = Flasherflash6.visible
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

'******************************************************
'*****   3D INSERTS
'******************************************************
'
'
' Before you get started adding the inserts to your playfield in VPX, there are a few things you need to have done to prepare:
' 	1. Cut out all the inserts on the playfield image so there is alpha transparency where they should be.
'      Make sure the playfield material has Opacity Active checkbox checked.
'	2. All the  insert text and/or images that lie over the insert plastic needs to be in its own file with 
'	   alpha transparency. Many playfields may require finding the original font and remaking the insert text.
' 
' To add the inserts:
'	1. Import all the textures (images) and materials from this file that start with the word "Insert" into your Table
'   2. Copy and past the two primitves that make up the insert you want to use. One primitive is for the on state, the other for the off state.
'   3. Align the primitives with the associated insert light. Name the on and off primitives correctly.
'   4. Update the Lampz object array. Follow the example in this file.
'   5. You will need to manually tweak the disable lighting value and material parameters to achielve the effect you want.
'
'
' Quick Reference:  Laying the Inserts ( Tutorial From Iaakki)
' - Each insert consists of two primitives. On and Off primitive. Suggested naming convention is to use lamp number in the name. For example 
'   is lamp number is 57, the On primitive is "p57" and the Off primitive is "p57off". This makes it easier to work on script side.
' - When starting from a new table, I'd first select to make few inserts that look quite similar. Lets say there is total of 6 small triangle 
'   inserts, 4 yellow and 2 blue ones.
' - Import the insert on/off images from the image manager and the vpx materials used from the sample project first, and those should appear 
'   selected properly in the primitive settings when you paste your actual insert trays in your target table . Then open up your target project 
'   at same time as the sample project and use copy&paste to copy desired inserts to target project. 
' - There are quite many parameters in primitive that affect a lot how they will look. I wouldn't mess too much with them. Use Size options to 
'   scale the insert properly into PF hole. Some insert primitives may have incorrect pivot point, which means that changing the depth, you may 
'   also need to alter the Z-position too.
' - Once you have the first insert in place, wire it up in the script (detailed in part 3 below). Then set the light bulb's intensity to zero, 
'   so it won't harass the adjustment.
' - Start up the game with F6 and visually see if the On-primitive blinks properly. If it is too dim, hit D and open editor. Write: 
' - p57.BlendDisableLighting = 300 and hit enter
' - -> The insert should appear differently. Find good looking brightness level. Not too bright as the light bulb is still missing. Just generic good light.
'     - If you cannot find proper light color or "mood", you can also fiddle with primitive material values. Provided material should be 
'       quite ok for most of the cases.
'     - Now when you have found proper DL value (165), but that into script:
'     - Lampz.Callback(57) = " DisableLighting p57, 165,"
' - That one insert is now adjusted and you should be able to copy&paste rest of the triangle inserts in place and name them correctly. And add them 
'   into script. And fine tune their brightness and color.
'
' Light bulbs and ball reflection:
' 
' - This kind of lighted primitives are not giving you ball reflections. Also some more glow vould be needed to make the insert to bloom correctly.
' - Take the original lamp (l57), set the bulb mode enabled, set Halo Height to -3 (something that is inside the 2 insert primitives). I'd start with 
'   falloff 100, falloff Power 2-2.5, Intensity 10, scale mesh 10, Transmit 5.
' - Start the game with F6, throw a ball on it and move the ball near the blinking insert. Visually see how the reflection looks.
' - Hit D once the reflection is the highest. Open light editor and start fine tuning the bulb values to achieve realistic look for the reflection.
' - Falloff Power value is the one that will affect reflection creatly. The higher the power value is, the brighter the reflection on the ball is. 
'   This is the reason why falloff is rather large and falloff power is quite low. Change scale mesh if reflection is too small or large.
' - Transmit value can bring nice bloom for the insert, but it may also affect to other primitives nearby. Sometimes one need to set transmit to 
'   zero to avoid affecting surrounding plastics. If you really need to have higher transmit value, you may set Disable Lighting From Below to 1 
'   in surrounding primitive. This may remove the problem, but can make the primitive look worse too.


'******************************************************
'*****   END 3D INSERTS
'******************************************************

' Flasher objects

Sub Flash(nr, object)
	flashm nr, Object
	FadeEmpty nr
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change anything, it just follows the main flasher
    Select Case FadingLevel(nr)
        Case 4, 5
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub


Sub FlupperFlash(nr, FlashObject, LitObject, BaseObject, LightObject)
	FlupperFlashm nr, FlashObject, LitObject, BaseObject, LightObject
	FadeEmpty nr
End Sub

Sub FlupperFlashm(nr, FlashObject, LitObject, BaseObject, LightObject)
	'exit sub
	dim flashx3
	Select Case FadingLevel(nr)
        Case 4, 5
			' This section adapted from Flupper's script
			flashx3 = FlashLevel(nr) * FlashLevel(nr) * FlashLevel(nr)
            FlashObject.IntensityScale = flashx3
			LitObject.BlendDisableLighting = 10 * flashx3
			BaseObject.BlendDisableLighting = flashx3
			LightObject.IntensityScale = flashx3 
			LitObject.material = "domelit" & Round(9 * FlashLevel(nr))
			LitObject.visible = 1
			FlashObject.visible = 1	
		case 3:
			LitObject.visible = 0
			FlashObject.visible = 0
	end select
End Sub

Sub FadeEmpty(nr)	'Fade a lamp number, no object updates
    Select Case FadingLevel(nr)
		Case 3
			FadingLevel(nr) = 0
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 3 'completely off
            End if
            'Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
            End if
            'Object.IntensityScale = FlashLevel(nr)
		Case 6
			FadingLevel(nr) = 1
    End Select
End Sub

Sub FlashBlink(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) <FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 0 AND FlashRepeat(nr) Then 'repeat the flash
                FlashRepeat(nr) = FlashRepeat(nr) -1
                If FlashRepeat(nr) Then FadingLevel(nr) = 5
            End If
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr)> FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 1 AND FlashRepeat(nr) Then FadingLevel(nr) = 4
    End Select
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 3:object.SetValue 3
    End Select
End Sub

'Texts

Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub


				'JP's VP10 Rolling Sounds

'      **** If I delete it then Elvis didnt dance ;)) ***
'*****************************************


Sub RollingUpdate()
End Sub


'******************************************************
'                BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 5 ' total number of balls
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

Sub RollingTimer_Timer()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If                                
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + offsetY
			BallShadowA(b).X = BOT(b).X + offsetX
			BallShadowA(b).visible = 1
		End If
	Next
End Sub


'******************
' RealTime Updates
'******************

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    RollingUpdate
    door.RotX = fdoor.CurrentAngle
    pRarm.Roty = fArms.CurrentAngle + 20
    pLarm.Roty = fArms.CurrentAngle
    pLegs.Rotx = fLegs.CurrentAngle
End Sub


'*************************
' GI - needs new vpinmame
'*************************

Set GICallback = GetRef("GIUpdate")

Sub GIUpdate(no, Enabled)
     If Enabled Then
        dim xx
        For each xx in GI:xx.State = 1: Next
		PlaySound "Relay_GI_On"
		Apron2.image = "apron_instr"
		Apron3.image = "apron_coin"
		Apron4.image = "apron_elvis"
		rampL_reflection1.visible = 1
		rampL_reflection2.visible = 1
		rampL_reflection3.visible = 1
		rampL_reflection4.visible = 1
		rampL_reflection5.visible = 1
		rampL_reflection6.visible = 1
		rampL_reflection7.visible = 1
		rampL_reflection8.visible = 1
		rampL_reflection9.visible = 1
		rampL_reflection10.visible = 1
		rampL_reflection11.visible = 1
		rampL_reflection12.visible = 1
		rampL_reflection13.visible = 1
		rampL_reflection14.visible = 1
		rampL_reflection15.visible = 1
		rampL_reflection16.visible = 1
		rampL_reflection17.visible = 1
		rampL_reflection18.visible = 1
		ramp_flasher1.visible = 1
		ramp_flasher2.visible = 1
		ramp_flasher3.visible = 1
		ramp_flasher4.visible = 1
		Reflector1_flasher.visible = 1
		Reflector2_flasher.visible = 1
		Reflector3_flasher.visible = 1
		plastic1.BlendDisableLighting = 0.2 
		plastic2.BlendDisableLighting = 0.2 
		Wall39.BlendDisableLighting = 0.2 'inlane left
		Wall40.BlendDisableLighting = 0.2 'inlane right
		backwall.BlendDisableLighting = 0.1 'backwall
		HoundDog.BlendDisableLighting = 0.2 
		door.BlendDisableLighting = 0.2 
		sw64.BlendDisableLighting = 0.3 
		Wall497.BlendDisableLighting = 0.1 'left flasher wall
		Wall494.BlendDisableLighting = 0.1 'hotel roof
		Wall68.BlendDisableLighting = 0.1 'hotel 1
		Wall69.BlendDisableLighting = 0.3 'hotel 2
		Ramp48.image = "graceland" '3 lights
		Ramp33.image = "bank5-1"
		Ramp17.image = "bank5-2" 
		Ramp50.image = "jail" 

		vegas1.image = "vegas"
		If VegasMod = 1 then 
			
			vegas_flasher1.visible = 1
			vegas_flasher2.visible = 1
			vegas_flasher3.visible = 1
		End if

		sideblade_right.visible = 1
		sideblade_left.visible = 1
		flasher23.visible=1
		flasher24.visible=1
		flasher25.visible=1
		flasher26.visible=1
		flasher27.visible=1
		flasher28.visible=1
		flasher29.visible=1
		flasher30.visible=1
		flasher30a.visible=1
		flasher31.visible=1
		flasher31a.visible=1
		flasher32.visible=1

		'Metal ramps
		Ramp12.image = "RL" 
		Ramp26.image = "R" 
		Ramp10.image = "R" 
		Ramp14.image = "RR" 
		Ramp59.image = "Metal Wall3" 
		Ramp70.image = "Metal Wall3" 
		Ramp77.image = "Metal Wall3a" 
		Ramp64.image = "Metal Wall3" 
		Ramp65.image = "Metal Wall3" 
		Ramp74.image = "Metal Wall3a" 
		Ramp69.image = "Metal Wall3" 
		Ramp71.image = "Metal Wall3" 
		Ramp9.image = "Metal Wall3a" 
		Ramp60.image = "metal" 
		Ramp66.image = "metal" 
		Ramp1.image = "metal" 
		Ramp4.image = "metal" 
		Ramp76.image = "metal" 
		Ramp6.image = "metal" 
		Ramp72.image = "metal" 
		Ramp7.image = "metal" 


  Else
        For each xx in GI:xx.State = 0: Next
		Playsound "Relay_GI_Off"
		Apron2.image = "apron_instr_off"
		Apron3.image = "apron_coin_off"
		Apron4.image = "apron_elvis_off"
		rampL_reflection1.visible = 0
		rampL_reflection2.visible = 0
		rampL_reflection3.visible = 0
		rampL_reflection4.visible = 0
		rampL_reflection5.visible = 0
		rampL_reflection6.visible = 0
		rampL_reflection7.visible = 0
		rampL_reflection8.visible = 0
		rampL_reflection9.visible = 0
		rampL_reflection10.visible = 0
		rampL_reflection11.visible = 0
		rampL_reflection12.visible = 0
		rampL_reflection13.visible = 0
		rampL_reflection14.visible = 0
		rampL_reflection15.visible = 0
		rampL_reflection16.visible = 0
		rampL_reflection17.visible = 0
		rampL_reflection18.visible = 0
		ramp_flasher1.visible = 0
		ramp_flasher2.visible = 0
		ramp_flasher3.visible = 0
		ramp_flasher4.visible = 0
		Reflector1_flasher.visible = 0
		Reflector2_flasher.visible = 0
		Reflector3_flasher.visible = 0
		plastic1.BlendDisableLighting = 0 
		plastic2.BlendDisableLighting = 0 
		Wall39.BlendDisableLighting = 0 'inlane left
		Wall40.BlendDisableLighting = 0 'inlane right
		backwall.BlendDisableLighting = 0 'backwall
		door.BlendDisableLighting = 0
		HoundDog.BlendDisableLighting = 0
		sw64.BlendDisableLighting = 0	
		Wall497.BlendDisableLighting = 0 'hotel roof
		Wall494.BlendDisableLighting = 0 'hotel roof
		Wall68.BlendDisableLighting = 0 'hotel 1
		Wall69.BlendDisableLighting = 0 'hotel 2
		Ramp48.image = "graceland_off" '3 lights
		Ramp33.image = "bank5-12" 
		Ramp17.image = "bank5-22" 
		Ramp50.image = "jail_off" 
		vegas1.image = "vegas_off"

		If VegasMod = 1 then 
			
			vegas_flasher1.visible = 0
			vegas_flasher2.visible = 0
			vegas_flasher3.visible = 0
		End if
		
		sideblade_right.visible = 0
		sideblade_left.visible = 0
		flasher23.visible=0
		flasher24.visible=0
		flasher25.visible=0
		flasher26.visible=0
		flasher27.visible=0
		flasher28.visible=0
		flasher29.visible=0
		flasher30.visible=0
		flasher30a.visible=0
		flasher31.visible=0
		Flasher31a.visible=0
		flasher32.visible=0

		'Metal ramps
		Ramp12.image = "RL_off" '
		Ramp26.image = "R_off" '
		Ramp10.image = "R_off" '
		Ramp14.image = "RR_off" '
		Ramp59.image = "Metal Wall3_off" 
		Ramp70.image = "Metal Wall3_off" 
		Ramp77.image = "Metal Wall3_off" 
		Ramp64.image = "Metal Wall3_off" 
		Ramp65.image = "Metal Wall3_off" 
		Ramp74.image = "Metal Wall3_off" 
		Ramp69.image = "Metal Wall3_off" 
		Ramp71.image = "Metal Wall3_off" 
		Ramp9.image = "Metal Wall3_off" 
		Ramp60.image = "metal_off" 
		Ramp66.image = "metal_off" 
		Ramp1.image = "metal_off" 
		Ramp4.image = "metal_off" 
		Ramp76.image = "metal_off" 
		Ramp6.image = "metal_off" 
		Ramp72.image = "metal_off" 
		Ramp7.image = "metal_off" 
		
    End If
End Sub


Sub Break_hit

Dim Bounce

	If ActiveBall.VelY > 13.5 Then
		PlaySound "metal_ramp_end1",0, 1, 0.15, 0.35:
		Bounce = Int((( BounceMax-BounceMin+1)*Rnd) + BounceMin) 'Random bounce force
		Debug.Print "VelY = " & ActiveBall.Vely & ", Bounce Force = " & Bounce
		ActiveBall.VelY = -Bounce
		ActiveBall.VelX = 0

        Break.Enabled = False	'disable trigger
		Break.TimerInterval = 2000 'msec
		Break.TimerEnabled = True
	Else
		PlaySound "metal_ramp_end",0, 1, 0.15, 0.35:
		Debug.Print "VelY = " & ActiveBall.Vely & ", No bounce back"
		ActiveBall.VelY = 0
		ActiveBall.VelX = 0
	End If
End Sub

Sub Break_Timer
	Break.Enabled = True			'enable trigger
	Break.TimerEnabled = False
End Sub


'******
' Rules
'******
Dim Msg(20)
Sub Rules()
    Msg(0) = "Elvis - Stern 2004" &Chr(10) &Chr(10)
    Msg(1) = ""
    Msg(2) = "OBJECTIVE:Get to Graceland by lighting the following:"
    Msg(3) = "*FEATURED HITS COMPLETED (start all 5 song modes)"
    Msg(4) = "   Hound Dog (Shoot HOUND DOG Target)"
    Msg(5) = "   Blue Suede Shoes (Shoot CENTER LOOP with Upper Right Flipper)"
    Msg(6) = "   Heartbreak Hotel (Shoot balls into HEARTBREAK HOTEL on Upper Playfield)"
    Msg(7) = "   Jailhouse Rock (Shoot balls into the JAILHOUSE EJECT HOLE)"
    Msg(8) = "   All Shook Up (Shoot ALL-SHOOK shots)"
    Msg(9) = "*GIFTS FROM ELVIS COMPLETED ~ Shoot E-L-V-I-S Drop Targets to light GIFT"
    Msg(10) = " FROM ELVIS on the TOP EJECT HOLE"
    Msg(11) = "*TOP TEN COUNTDOWN COMPLETED ~ Shoot lit'music lights's to advance TOP"
    Msg(12) = " 10 COUNTDOWN."
    Msg(13) = "SKILL SHOT: Plunge ball in the WLVS Top Lanes or E-L-V-I-S Drop Targets"
    Msg(14) = "MYSTERY:Ball in the Pop Bumpers will change channels until all 3 TVs match."
    Msg(15) = "EXTRA BALL: Shoot Right Ramp ro light Extra Ball."
    Msg(16) = "TCB: Complete T-C-B to double all scoring."
    Msg(17) = "ENCORE: Spell E-N-C-O-R-E (letters lit in Back Panel) to earn"
    Msg(18) = "an Extra Multiball after the game."
    Msg(19) = ""
    Msg(20) = ""
    For X = 1 To 20
        Msg(0) = Msg(0) + Msg(X) &Chr(13)
    Next

    MsgBox Msg(0), , "         Instructions and Rule Card"
End Sub


'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1                                                                                                                'volume level; range [0, 1]
NudgeLeftSoundLevel = 1                                                                                                        'volume level; range [0, 1]
NudgeRightSoundLevel = 1                                                                                                'volume level; range [0, 1]
NudgeCenterSoundLevel = 1                                                                                                'volume level; range [0, 1]
StartButtonSoundLevel = 0.1                                                                                                'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr                                                                                        'volume level; range [0, 1]
PlungerPullSoundLevel = 1                                                                                                'volume level; range [0, 1]
RollingSoundFactor = 1.1/5                

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010                                                           'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635                                                                'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                                                                        'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                                                                      'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
SlingshotSoundLevel = 0.95                                                                                                'volume level; range [0, 1]
BumperSoundFactor = 8                                                                                                'volume multiplier; must not be zero
KnockerSoundLevel = 0.5                                                                                                         'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2                                                                        'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5                                                                                        'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5                                                                                        'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5                                                                                'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025                                                                        'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025                                                                        'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8                                                                        'volume level; range [0, 1]
WallImpactSoundFactor = 0.075                                                                                        'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5                                                                                                        'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                                                                                        'volume multiplier; must not be zero
DTSoundLevel = 0.5                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                                                                      'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                                                                      'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 1                                                                                                'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                                                                        'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                                                                                'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                                                                                                        'volume multiplier; must not be zero


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
        PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*7)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*7)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
        PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
        PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315                                                                        'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05                                                                        'volume level; range [0, 1];

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
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////



'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
        public ballvel, ballvelx, ballvely

        Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

        Public Sub Update()        'tracks in-ball-velocity
                dim str, b, AllBalls, highestID : allBalls = getballs

                for each b in allballs
                        if b.id >= HighestID then highestID = b.id
                Next

                if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
                if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
                if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

                for each b in allballs
                        ballvel(b.id) = BallSpeed(b)
                        ballvelx(b.id) = b.velx
                        ballvely(b.id) = b.vely
                Next
        End Sub
End Class

Sub RDampen_Timer()
        Cor.Update
End Sub

' VR PLUNGER ANIMATION

Sub TimerPlunger_Timer

	If PinCab_Shooter.Y < 26.34 then
		PinCab_Shooter.Y = PinCab_Shooter.Y + 5
	End If

End Sub

Sub TimerPlunger2_Timer
	PinCab_Shooter.Y = -73.66 + (5* Plunger.Position) -20
End Sub


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-10000) if you want to see the pf shadow through the ramp

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
' It's recommended that you be selective in which lights go in this collection, as there are limitations:
' 1. The shadows can "pass through" solid objects and other light sources, so be mindful of where the lights would actually able to cast shadows
' 2. If there are more than two equidistant sources, the shadows can suddenly switch on and off, so places like top and bottom lanes need attention
' 3. At this time the shadows get the light on/off from tracking gilvl, so if you have lights you want shadows for that are on at different times you will need to either:
'	a) remove this restriction (shadows think lights are always On)
'	b) come up with a custom solution (see TZ example in script)
' After confirming the shadows work in general, use ball control to move around and look for any weird behavior

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
' Example timer sub:

'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top for easy access by players
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' *** Change gBOT to BOT if using existing getballs code
' *** Includes lines commonly found there, for reference:
'	' stop the sound of deleted balls
'	For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
' ...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If gBOT(b).Z > 30 Then
'				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'			Else
'				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + offsetY
'			BallShadowA(b).X = gBOT(b).X + offsetX
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

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

'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 0		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(5), objrtx2(5)
dim objBallShadow(5)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

Dim ClearSurface:ClearSurface = True		'Variable for hiding flasher shadow on wire and clear plastic ramps
									'Intention is to set this either globally or in a similar manner to RampRolling sounds

'Initialization
DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

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
		If Not ClearSurface Then
			BallShadowA(num).visible = 1
			objBallShadow(num).visible = 0
		Else
			objBallShadow(num).visible = 1
		End If
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim gBOT: gBOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update

				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					If gBOT(s).X < tablewidth/2 Then
						objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
					Else
						objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
					End If
					objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				End If

			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s

				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf

			Else												'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY
				End If
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=0.1
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff Then
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
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


' version 0.1alt - Drop-in replacement of DynamicLamps, but stripped down For PWM pinmame update. removed all fading, objects are simply updated on a timer
' Setup: 
'	.MassAssign index, object or array of objects	-- Appends flasher or lamp object to a lamp index. For other objects, see '.Callback'
'	.MassAssign(index) = object or array of objects	-- same as above
'	.Init	--	Optional, makes sure all currently assigned lamp object have their states set to 1
'	.Update	--	must be set

' Usage: 
'	.SetLamp index value	--	 set a lamp index
'	.SetModLamp index value	--	set a lamp index - input will be divided by 255
'	.state(index) = value	--	same as SetLamp but uses a different syntax

' Special features: 
'	.Callback = "methodname" -- calls a method with current fading value (0 to 1 float value). For handling .blenddisablelighting or other lighting tricks
'	.Mult(index) = value  -- sets a multiplier on the lighting, for balancing lamps against each GI or other tricks
'	.Filter = "functionname" -- puts *all* lamp indexes through a function. Function must take a numeric value and return a numeric value

Class NoFader2 'Lamps that fade up and down. GI and Flasher handling for PWM pinmame update
	Private UseCallback(32), cCallback(32)
	private Lvl(32)
	private Obj(32)
	private Lock(32)
	private Mult(32)
	'Private UseFunction, cFilter

	Private Sub Class_Initialize() 
		dim x : for x = 0 to uBound(Lock) 
			mult(x) = 1
			Lock(x)=True
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader
		next
	End Sub

	Public Sub Init() : TurnOnStates : end sub	'just call turnonstates for now

	Public Property Let Callback(aIdx, String) : cCallback(aIdx) = String : UseCallBack(aIdx) = True : End Property 'execute
	'Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	'Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	'solcallback (solmodcallback) handler 
	Public Sub SetLamp(aIdx, aInput) : Lvl(aIdx) = aInput: Lock(aIdx)=false : End Sub	'0->1 Input
	Public Sub SetModLamp(aIdx, aInput) : Lvl(aIdx) = aInput/255: Lock(aIdx)=false : End Sub	'0->255 Input
	REM Sub SetModLamp(aIdx, aInput) : Update aIDX, aInput/255 : arrayFLASH(aIDX) = aInput : End Sub	'0->255 Input DEBUG

	Public Property Let State(aIdx, aInput) : Lvl(aIdx) = aInput: Lock(aIdx)=false : End Property
	Public Property Get state(aIdx ) : state = Lvl(aIdx) : end Property

	'Mass assign, Builds arrays where necessary
'	Public Sub MassAssign(aIdx, aInput)
'		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
'			if IsArray(aInput) then
'				obj(aIdx) = aInput
'			Else
'				Set obj(aIdx) = aInput
'			end if
'		Else
'			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
'		end if	
'	end Sub
	
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

	Private Sub TurnOnStates()	'If obj contains any light objects, set their states to 1
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.0000001
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.0000001
			end if
		Next
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1
		
	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Update aIdx, Lvl(aIdx) : End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property 


	Public Sub Update()	'Handle object updates. Call on timer 
		dim x,xx : for x = 0 to uBound(Obj)
			if not Lock(x) then
				Lock(x)=True
				if IsArray(obj(x)) Then
					for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
				else 
					obj(x).IntensityScale = Lvl(x)*Mult(x)
				end If

				If UseCallBack(x) then 
'					msgbox "execute" & cCallback(x) & " " & (Lvl(x))
					'execute cCallback(x) & " " & (Lvl(x))
					proc cCallback(x), (Lvl(x))
					
				end If
			end if
		Next
		
	End Sub	
	
	
End Class


'Helper function 
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

'Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

' version 0.1 - Drop-in replacement of DynamicLamps, but stripped down For PWM pinmame update. removed all fading, objects are simply updated as values come in
' Setup: 
'	.MassAssign index, object or array of objects	-- Appends flasher or lamp object to a lamp index. For other objects, see '.Callback'
'	.MassAssign(index) = object or array of objects	-- same as above
'	.Init	--	Optional, makes sure all currently assigned lamp object have their states set to 1

' Usage: 
'	.SetLamp index value	--	 set a lamp index
'	.SetModLamp index value	--	set a lamp index - input will be divided by 255
'	.state(index) = value	--	same as SetLamp but uses a different syntax

' Special features: 
'	.Callback = "methodname" -- calls a method with current fading value (0 to 1 float value). For handling .blenddisablelighting or other lighting tricks
'	.Mult(index) = value  -- sets a multiplier on the lighting, for balancing lamps against each GI or other tricks
'	.Filter = "functionname" -- puts *all* lamp indexes through a function. Function must take a numeric value and return a numeric value

Class NoFader 'Lamps that fade up and down. GI and Flasher handling for PWM pinmame update
	Private UseCallback(32), cCallback(32)
	Public Lvl(32)
	Public Obj(32)
	Private UseFunction, cFilter
	private Mult(32)

	Private Sub Class_Initialize() 
		dim x : for x = 0 to uBound(Obj) 
			mult(x) = 1
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader
		next
	End Sub

	Public Sub Init() : TurnOnStates : end sub	'just call turnonstates for now

	Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property 'execute
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	'solcallback (solmodcallback) handler 
	Public Sub SetLamp(aIdx, aInput) : Update aIDX, aInput : End Sub	'0->1 Input
	REM Public Sub SetModLamp(aIdx, aInput) : Update aIDX, aInput/255 : End Sub	'0->255 Input
	Public Sub SetModLamp(aIdx, aInput) : Update aIDX, aInput/255 : arrayFLASH(aIDX) = aInput : End Sub	'0->255 Input DEBUG

	Public Property Let State(idx,Value) : Update idx, Value : End Property
	Public Property Get state(idx) : state = Lvl(idx) : end Property

	'Mass assign, Builds arrays where necessary
	'Public Sub MassAssign(aIdx, aInput)
	'	If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
	'		if IsArray(aInput) then
	'			obj(aIdx) = aInput
	'		Else
	'			Set obj(aIdx) = aInput
	'		end if
	'	Else
	'		Obj(aIdx) = AppendArray(obj(aIdx), aInput)
	'	end if	
	'end Sub
	
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

	Private Sub TurnOnStates()	'If obj contains any light objects, set their states to 1
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.0000001
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.0000001
			end if
		Next
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1
		
	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Update aIdx, Lvl(aIdx) : End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property 

	Private Sub Update(aIDX, aLVL) ' NOT called on a timer!
		Lvl(aIDX) = aLVL ' keep state for reference
		dim xx
		if IsArray(obj(aIDX) ) Then	'if array
			If UseFunction then 
				for each xx in obj(aIDX) : xx.IntensityScale = cFilter(aLVL*mult(aIDX)) : Next
			Else
				for each xx in obj(aIDX) : xx.IntensityScale = aLVL*mult(aIDX) : Next
			End If
		else						'if single lamp or flasher
			If UseFunction then 
				obj(aIDX).Intensityscale = cFilter(aLVL*mult(aIDX))
			Else
				obj(aIDX).Intensityscale = aLVL*mult(aIDX)
			End If
		end if
'		If UseCallBack(aIDX) then execute cCallback(aIDX) & " " & (aLVL*mult(aIDX))	'Callback (execute)
		If UseCallBack(aIDX) then proc cCallback(aIDX), (aLVL*mult(aIDX))	'Callback (execute)

	End Sub
	
End Class


'Helper function 
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

'Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class
