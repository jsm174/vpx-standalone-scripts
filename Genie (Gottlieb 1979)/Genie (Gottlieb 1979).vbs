'************************************************************
'************************************************************
'
'  Genie / IPD No. 997 / October, 1979 / 4 Players
'
'   Credits:
'
'		VPX by bord, rothbauerw
'	   
'		VR Rooms - TOTAN by Flupper, 360 added by Rajo Joey, Minimal added by Rajo Joey
'		VR Cabinet - Rajo Joey, rothbauerw
'		VR/Backglass - Backglass by blacksad, converted by rothbauerw
'
'
'************************************************************
'************************************************************ 

Option Explicit
Randomize

'******************************************************
' 						OPTIONS
'******************************************************

' Use Magnasave to select between Totan, 360, and Minimal Rooms
Const VRRoom = 0				'1 - VR, '0 - Desktop, Fullscreen, FSS
Const CabinetMode = 0			'1 - hides backbox, rails, and lockdown bar

' Enable Drop Target Bricking
Const DTEnableBrick = 0			'0 - disable, 1 - enable
' Enhance micro bounces on flippers
Const Rubberizer = 1			'0 - disable, 1 - rothbauerw version, 2 - iaakki version

Const LibOrCons	= 2				'1 for Liberal, 2 for Conservative (liberal also needs pf image set to pf_lib)
Const ChimesOn = 0				'1 to use Chimes, 0 to turn them off

'///////////////////////-----General Sound Options-----///////////////////////
'//  VolumeDial:
'//  VolumeDial is the actual global volume multiplier for the mechanical sounds.
'//  Values smaller than 1 will decrease mechanical sounds volume.
'//  Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'******************************************************
' 					STANDARD DEFINITIONS
'******************************************************

Dim BallMass ,BallSize
Ballsize = 50
Ballmass = 1.0

Const UseSolenoids=2
Const UseLamps=1
Const UseSync=1
Const UseGI=0

' Standard Sounds

Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""


'******************************************************
' 					TABLE INIT
'******************************************************

On Error Resume Next
	ExecuteGlobal GetTextFile("controller.vbs")
	If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM"01120100","GTS1.VBS",3.02

Const cGameName="genie"

Dim DesktopMode: DesktopMode = Table1.ShowDT

' using table width and height in script slows down the performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

'*********** Desktop/Cabinet settings ************************

Dim GenieBall, xx

Sub Table1_Init
	vpmInit Me

	With Controller
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine="Genie (Gottlieb 1979)"
		.HandleKeyboard=0
		.ShowTitle=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.Hidden = 0
		On Error Resume Next
		.SolMask(0) = 0
		vpmTimer.AddTimer 1000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 1 seconds
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0	
	End With

	' Nudging
	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled = 1

	vpmNudge.TiltSwitch = 4
	vpmNudge.Sensitivity = 3
	vpmNudge.TiltObj = Array(LeftSlingshot, Bumper1, Bumper2, Bumper3)

	Set GenieBall = Drain.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Controller.Switch(66) = 1

	If LibOrCons = 1 Then
		For each xx in physics_lib
			xx.collidable = True
		Next
		For each xx in visible_lib
			xx.visible = True
		Next

		For each xx in physics_cons
			xx.collidable = false
		Next
		For each xx in visible_cons
			xx.visible = False
		Next
	Else
		For each xx in physics_lib
			xx.collidable = False
		Next
		For each xx in visible_lib
			xx.visible = False
		Next

		For each xx in physics_cons
			xx.collidable = True
		Next
		For each xx in visible_cons
			xx.visible = True
		Next
	End If

	If VRRoom = 1 then
		VRRoomSel = LoadValue("Genie", "V1.0.0")
		If VRRoomSel = "" Then VRRoomSel = 2
		VRChangeRoom
	End If

	If CabinetMode = 1 Then
		VR_Backbox.visible=False
		VR_SideRails.visible=False
		VR_Lockdownbar.visible=False
	End If

End Sub 


Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_Exit:Controller.Stop:End Sub

'******************************************************
' 						KEYS
'******************************************************

dim plungerpress

Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then Plunger.Pullback : SoundPlungerPull(): plungerpress = 1

	If keycode = LeftFlipperKey Then 
		FlipperActivate LeftFlipper, LFPress
		FlipperActivate LeftFlipper1, LFPress1
		VR_CabFlipperLeft.transx = 10
	End If

	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress
		FlipperActivate RightFlipper1, RFPress1
		FlipperActivate RightFlipper2, RFPress2
		VR_CabFlipperRight.transx = -10
	End If

	If keycode = StartGameKey Then 
		SoundStartButton
		VR_Cab_StartButton.transx = -5
	End If

' Change VR-Room with magna-save buttons
	If VRRoom = 1 Then
		If keycode = leftmagnasave then
			vrroomsel = vrroomsel - 1
		if vrroomsel < 0 then vrroomsel = 2
			VRChangeRoom
		End If

		If keycode = rightmagnasave then
			vrroomsel = vrroomsel + 1
		if vrroomsel > 2 then vrroomsel = 0
			VRChangeRoom
		End If
	End If

	If keycode = LeftTiltKey Then Nudge 90, 4 : SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 4 : SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 5 : SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 or keycode = AddCreditKey Then
		Select Case Int(rnd*3)
			Case 0: PlaySoundAtLevelStatic ("Coin_In_1"), CoinSoundLevel, Drain
			Case 1: PlaySoundAtLevelStatic ("Coin_In_2"), CoinSoundLevel, Drain
			Case 2: PlaySoundAtLevelStatic ("Coin_In_3"), CoinSoundLevel, Drain
		End Select
	End If

	If vpmKeyDown(KeyCode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then 
		Plunger.Fire
		If GenieBall.x > 1175 and GenieBall.y > 1910 Then	'If true then ball in shooter lane, else no ball is shooter lane
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		 plungerpress = 0
	End If

	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		FlipperDeActivate LeftFlipper1, LFPress1
		VR_CabFlipperLeft.transx = 0
	End If
	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		FlipperDeActivate RightFlipper1, RFPress1
		FlipperDeActivate RightFlipper2, RFPress2
		VR_CabFlipperRight.transx = 0
	End If

	If keycode = StartGameKey Then 		
		VR_Cab_StartButton.transx = 0
	End If

	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

'******************************************************
' 						SOLENOIDS
'******************************************************

SolCallback(1)	= "SolRelease"
SolCallback(2)	= "SolKnocker"
SolCallback(3)	= "SolChime1"
SolCallback(4)	= "SolChime2"
SolCallback(5)	= "SolChime3"
SolCallback(6)	= "SollSaucer"
SolCallback(7)	= "SolRightDrop"
SolCallback(8)	= "SolLeftDrop"
SolCallback(17)	= "vpmNudge.SolGameOn"

'******************************************************
' 						Chimes (Optional)
'******************************************************

Sub solchime1(Enable)
    If Enable then
		If ChimesOn=1 Then
			PlaySound SoundFX("sj_chime_10a",DOFChimes), 1, 1
			DOF 125, 2
		End If
	End If
End Sub

Sub solchime2(Enable)
    If Enable then
		If ChimesOn=1 Then
			PlaySound SoundFX("sj_chime_100a",DOFChimes), 1, 1
			DOF 126, 2
		End If
	End If
End Sub

Sub solchime3(Enable)
    If Enable then
		If ChimesOn=1 Then
			PlaySound SoundFX("sj_chime_1000a",DOFChimes), 1, 1
			DOF 127, 2
		End If
	End If
End Sub

'******************************************************
'					KNOCKER
'******************************************************
Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub

'******************************************************
'					FLIPPERS
'******************************************************

Const ReflipAngle = 20
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
		LeftFlipper1.RotateToEnd
	
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If

		If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper1
		Else 
			SoundFlipperUpAttackLeft LeftFlipper1
			RandomSoundFlipperUpLeft LeftFlipper1
		End If


		PlaySoundAtVolLoops "buzzL",LeftFlipper,0.05,-1
	Else
		LeftFlipper.RotateToStart
		LeftFlipper1.RotateToStart

		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If

		If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper1
		End If

		FlipperLeftHitParm = FlipperUpSoundLevel

		Stopsound "buzzL"
    End If
End Sub
 
Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire
		RightFlipper2.RotateToEnd
		RF1.fire

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If

		If rightflipper1.currentangle > rightflipper1.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper1
		Else 
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If

		If rightflipper2.currentangle > rightflipper2.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper2
		Else 
			SoundFlipperUpAttackRight RightFlipper2
			RandomSoundFlipperUpRight RightFlipper2
		End If

		PlaySoundAtVolLoops "buzz",RightFlipper,0.05,-1			
	Else
		RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
		RightFlipper2.RotateToStart

		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	

		If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper1
		End If	

		If RightFlipper2.currentangle > RightFlipper2.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper2
		End If	

		FlipperRightHitParm = FlipperUpSoundLevel

		StopSound "buzz"
	End If
End Sub

'******************************************************
'		DROP TARGETS INITIALIZATION
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
Dim DT20, DT21, DT23, DT24, DT30, DT70, DT31, DT71, DT60, DT74, DT61

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
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
'
'	Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 

' Left Bank
Set DT20 = (new DropTarget)(sw20, csw20, layer3sw20, 20, 0, false)
Set DT21 = (new DropTarget)(sw21, csw21, layer3sw21, 21, 0, false)
Set DT23 = (new DropTarget)(sw23, csw23, layer3sw23, 23, 0, false)
Set DT24 = (new DropTarget)(sw24, csw24, layer3sw24, 24, 0, false)
Set DT30 = (new DropTarget)(sw30, csw30, layer3sw30, 30, 0, false)
Set DT70 = (new DropTarget)(sw70, csw70, layer3sw70, 70, 0, false)
Set DT31 = (new DropTarget)(sw31, csw31, layer3sw31, 31, 0, false)
Set DT71 = (new DropTarget)(sw71, csw71, layer3sw71, 71, 0, false)
Set DT60 = (new DropTarget)(sw60, csw60, layer3sw60, 60, 0, false)
Set DT74 = (new DropTarget)(sw74, csw74, layer3sw74, 74, 0, false)
Set DT61 = (new DropTarget)(sw61, csw61, layer3sw61, 61, 0, false)

'Add all the Drop Target Arrays to Drop Target Animation Array
' DTAnimationArray = Array(DT1, DT2, ....)
Dim DTArray
DTArray = Array(DT20, DT21, DT23, DT24, DT30, DT70, DT31, DT71, DT60, DT74, DT61)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 			'in milliseconds
Const DTDropUpSpeed = 40 			'in milliseconds
Const DTDropUnits = 43 			'VP units primitive drops
Const DTDropUpUnits = 4 			'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 				'max degrees primitive rotates when hit
Const DTDropDelay = 20	 		'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40	 		'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30				'velocity at which the target will brick, set to '0' to disable brick

' Moved to Options Const DTEnableBrick = 1			'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "targethit"	'Drop Target Hit sound
Const DTDropSound = "DTDrop"		'Drop Target Drop sound
Const DTResetSound = "DTReset"	'Drop Target reset sound

Const DTMass = 0.2				'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				DROP TARGETS FUNCTIONS
'******************************************************

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

'Add Timer name DTAnim to editor to handle drop target animations
DTAnim.interval = 10
DTAnim.enabled = True

Sub DTAnim_Timer()
	DoDTAnim
	DoSTAnim

	If Layer3sw20.transz < -DTDropUnits/2 Then Flasher20.visible = 0 else Flasher20.visible = 1
	If Layer3sw21.transz < -DTDropUnits/2 Then Flasher21.visible = 0 else Flasher21.visible = 1
	If Layer3sw23.transz < -DTDropUnits/2 Then Flasher23.visible = 0 else Flasher23.visible = 1
	If Layer3sw24.transz < -DTDropUnits/2 Then Flasher24.visible = 0 else Flasher24.visible = 1
	If Layer3sw30.transz < -DTDropUnits/2 Then Flasher30.visible = 0 else Flasher30.visible = 1
	If Layer3sw70.transz < -DTDropUnits/2 Then Flasher70.visible = 0 else Flasher70.visible = 1
	If Layer3sw31.transz < -DTDropUnits/2 Then Flasher31.visible = 0 else Flasher31.visible = 1
	If Layer3sw71.transz < -DTDropUnits/2 Then Flasher71.visible = 0 else Flasher71.visible = 1
	If Layer3sw60.transz < -DTDropUnits/2 Then Flasher60.visible = 0 else Flasher60.visible = 1
	If Layer3sw74.transz < -DTDropUnits/2 Then Flasher74.visible = 0 else Flasher74.visible = 1
	If Layer3sw61.transz < -DTDropUnits/2 Then Flasher61.visible = 0 else Flasher61.visible = 1

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
	dim transz
	Dim animtime, rangle
	Dim disablerotx:disablerotx=0

	rangle = prim.rotz * 3.1416 / 180

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
		If disablerotx = 0 Then
			prim.rotx = DTMaxBend * cos(rangle)
			prim.roty = DTMaxBend * sin(rangle)
		End If
		DTAnimate = animate
		Exit Function
	elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		If disablerotx = 0 Then
			prim.rotx = DTMaxBend * cos(rangle)
			prim.roty = DTMaxBend * sin(rangle)
		End If
		animate = 2
		PlaySoundAt SoundFX(DTDropSound,DOFDropTargets),primary
	End If

	if animate = 2 Then
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
		end if

		If disablerotx = 0 Then
			prim.rotx = DTMaxBend * cos(rangle)/2
			prim.roty = DTMaxBend * sin(rangle)/2
		End If

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			prim.blenddisablelighting = 0.2
			secondary.collidable = 0
			controller.Switch(Switch) = 1
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
		If disablerotx = 0 Then
			prim.rotx = DTMaxBend * cos(rangle)
			prim.roty = DTMaxBend * sin(rangle)
		End If
	elseif animate = 3 and animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		If disablerotx = 0 Then
			prim.rotx = 0
			prim.roty = 0
		End If
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
			prim.blenddisablelighting = 0.35
			prim.transz = transz
		elseif transz > 0 then
			prim.transz = transz
		end if

		if prim.transz > DTDropUpUnits then 
			DTAnimate = -2
			If disablerotx = 0 Then
				prim.rotx = 0
				prim.roty = 0
			End If
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		controller.Switch(Switch) = 0

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

'******************************************************
'					DROP TARGETS
'******************************************************

Sub sw20_Hit:DTHit 20:End Sub
Sub sw21_Hit:DTHit 21:End Sub
Sub sw23_Hit:DTHit 23:End Sub
Sub sw24_Hit:DTHit 24:End Sub
Sub sw30_Hit:DTHit 30:End Sub
Sub sw70_Hit:DTHit 70:End Sub
Sub sw31_Hit:DTHit 31:End Sub
Sub sw71_Hit:DTHit 71:End Sub
Sub sw60_Hit:DTHit 60:End Sub
Sub sw74_Hit:DTHit 74:End Sub
Sub sw61_Hit:DTHit 61:End Sub

Sub SolRightDrop(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors), sw23
		DTRaise 20
		DTRaise 21
		DTRaise 23
		DTRaise 24
	End if
End Sub

Sub SolLeftDrop(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors), sw71
		DTRaise 30
		DTRaise 70
		DTRaise 31
		DTRaise 71
		DTRaise 60
		DTRaise 74
		DTRaise 61
	End If
End Sub


'******************************************************
'		DROP TARGET
'		SUPPORTING FUNCTIONS 
'******************************************************

' Used for drop targets
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


Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
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
Dim ST40, ST44, ST51, ST63

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0

Set ST40 = (new StandupTarget)(sw40, Layer3sw40,40, 0)
Set ST44 = (new StandupTarget)(sw44, Layer3sw44,44, 0)
Set ST51 = (new StandupTarget)(sw51, Layer3sw51,51, 0)
Set ST63 = (new StandupTarget)(sw63, Layer3sw63,63, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST40, ST44, ST51, ST63)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit
Const STHitSound = "targethit"	'Stand-up Target Hit sound

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

	PlayTargetSound
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).sw = switch Then STArrayID = i:Exit Function 
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

'***********************************************************
'****				STAND UP TARGET CODE				****
'***********************************************************

Sub sw40_Hit: STHit 40:end sub
Sub sw44_Hit: STHit 44:end sub
Sub sw51_Hit: STHit 51:end sub
Sub sw63_Hit: STHit 63:DOF 111, DOFPulse:End Sub


'******************************************************
'					DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	Controller.Switch(66) = 1
	RandomSoundDrain Drain
End Sub

Sub Drain_UnHit()  'Drain
	Controller.Switch(66) = 0
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		RandomSoundBallRelease drain
		Drain.kick 60, 20		
	End If
End Sub

'******************************************************
'						KICKER
'******************************************************

'*** PI returns the value for PI
Function PI()
	PI = 4*Atn(1)
End Function

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180

	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

Sub TopHole_hit
	SoundSaucerLock

	set KickerBall = activeball	
	Controller.switch(41)=1
End Sub

Sub TopHole_unHit
	Controller.Switch(41) = 0
End Sub

Dim KickerBall, kickstep1

Sub SolLSaucer(Enable)
    If Enable then
		If controller.Switch(41) = True Then
			SoundSaucerKick 1, tophole
		Else
			SoundSaucerKick 0, tophole
		End If

		kickstep1 = 0
		pkickarm.rotz=12
		If tophole.ballcntover > 0 then
			KickBall KickerBall, 208, 9, 5, 30
		End If
		tophole.timerenabled = 1
    End If
End Sub

Sub tophole_timer
		Select Case kickstep1
        Case 3:pkickarm.rotz=12
        Case 4:pkickarm.rotz=12
        Case 5:pkickarm.rotz=12
        Case 6:pkickarm.rotz=12
        Case 7:pkickarm.rotz=4
        Case 8:pkickarm.rotz=2
		Case 9:pkickarm.rotz=0:tophole.TimerEnabled = 0: if tophole.ballcntover > 0 then SolLSaucer -1
	End Select
	kickstep1 = kickstep1 + 1
End Sub

'******************************************************
'					TRIGGERS & TARGETS
'******************************************************

'***********************************************************
'****			Rollover Wire Trigger CODE				****
'***********************************************************
Sub SW10A_Hit:Controller.Switch(10)=1 : RandomSoundRollover : DOF 103, DOFOn: End Sub
Sub SW10A_UnHit:Controller.Switch(10)=0:DOF 103, DOFOff:End Sub
Sub SW11A_Hit:Controller.Switch(11)=1 : RandomSoundRollover : DOF 105, DOFOn : End Sub
Sub SW11A_UnHit:Controller.Switch(11)=0:DOF 105, DOFOff :End Sub
Sub SW13A_Hit:Controller.Switch(13)=1 : RandomSoundRollover : DOF 107, DOFOn : End Sub
Sub SW13A_UnHit:Controller.Switch(13)=0:DOF 107, DOFOff :End Sub
Sub SW14A_Hit:Controller.Switch(14)=1 : RandomSoundRollover : DOF 109, DOFOn : End Sub
Sub SW14A_UnHit:Controller.Switch(14)=0:DOF 109, DOFOff :End Sub

Sub SW10B_Hit:Controller.Switch(10)=1 : RandomSoundRollover : DOF 104, DOFOn :End Sub
Sub SW10B_UnHit:Controller.Switch(10)=0:DOF 104, DOFOff :End Sub
Sub SW11B_Hit:Controller.Switch(11)=1 : RandomSoundRollover : DOF 106, DOFOn : End Sub
Sub SW11B_UnHit:Controller.Switch(11)=0:DOF 106, DOFOff :End Sub
Sub SW13B_Hit:Controller.Switch(13)=1 : RandomSoundRollover : DOF 108, DOFOn :End Sub
Sub SW13B_UnHit:Controller.Switch(13)=0:DOF 108, DOFOff :End Sub
Sub SW14B_Hit:Controller.Switch(14)=1 : RandomSoundRollover : DOF 110, DOFOn :End Sub
Sub SW14B_UnHit:Controller.Switch(14)=0:DOF 110, DOFOff :End Sub

Sub SW54A_Hit:Controller.Switch(54)=1 : RandomSoundRollover :DOF 115, DOFOn : End Sub
Sub SW54A_UnHit:Controller.Switch(54)=0:DOF 115, DOFOff :End Sub
Sub SW54B_Hit:Controller.Switch(54)=1 : RandomSoundRollover :DOF 116, DOFOn : End Sub
Sub SW54B_UnHit:Controller.Switch(54)=0:DOF 116, DOFOff :End Sub

Sub SW63A_Hit:Controller.Switch(63)=1 : RandomSoundRollover :DOF 112, DOFOn : End Sub
Sub SW63A_UnHit:Controller.Switch(63)=0:DOF 112, DOFOff :End Sub
Sub SW63B_Hit:Controller.Switch(63)=1 : RandomSoundRollover :DOF 113, DOFOn : End Sub
Sub SW63B_UnHit:Controller.Switch(63)=0:DOF 113, DOFOff :End Sub
Sub SW63C_Hit:Controller.Switch(63)=1 : RandomSoundRollover :DOF 114, DOFOn : End Sub
Sub SW63C_UnHit:Controller.Switch(63)=0:DOF 114, DOFOff :End Sub

Sub SW73A_Hit:Controller.Switch(73)=1 : RandomSoundRollover:DOF 117, DOFOn : End Sub
Sub SW73A_UnHit:Controller.Switch(73)=0:DOF 117, DOFOff :End Sub
Sub SW73B_Hit:Controller.Switch(73)=1 : RandomSoundRollover :DOF 118, DOFOn : End Sub
Sub SW73B_UnHit:Controller.Switch(73)=0:DOF 118, DOFOff :End Sub

'***********************************************************
'****				Star SWITCH CODE				****
'***********************************************************
Sub SW53A_Hit:Controller.Switch(53)=1 : RandomSoundRollover : DOF 122, DOFOn :star53a.transz=-4 : End Sub 
Sub SW53A_UnHit:Controller.Switch(53)=0:star53a.transz=0 :DOF 122, DOFOff : End Sub 
Sub SW53B_Hit:Controller.Switch(53)=1 : RandomSoundRollover : DOF 123, DOFOn :star53b.transz=-4 : End Sub 
Sub SW53B_UnHit:Controller.Switch(53)=0:star53b.transz=0 :DOF 123, DOFOff : End Sub 
Sub SW53C_Hit:Controller.Switch(53)=1 : RandomSoundRollover :DOF 124, DOFOn : star53c.transz=-4 : End Sub 
Sub SW53C_UnHit:Controller.Switch(53)=0:star53c.transz=0 :DOF 124, DOFOff : End Sub 
Sub SW64A_Hit:Controller.Switch(64)=1 : RandomSoundRollover : DOF 119, DOFOn :star64a.transz=-4 : End Sub 
Sub SW64A_UnHit:Controller.Switch(64)=0:star64a.transz=0 :DOF 119, DOFOff : End Sub 
Sub SW64B_Hit:Controller.Switch(64)=1 : RandomSoundRollover : DOF 120, DOFOn :star64b.transz=-4 : End Sub 
Sub SW64B_UnHit:Controller.Switch(64)=0:star64b.transz=0 :DOF 120, DOFOff : End Sub 

'***********************************************************
'****					SPINNER CODE					****
'***********************************************************
Sub SW33_Spin:vpmTimer.PulseSw 33 : playsoundat"fx_spinner", sw33 : End Sub

'***********Rotate Spinner
'Dim Angle

Sub SpinnerTimer_Timer
	SpinnerP.Rotx = sw33.CurrentAngle
	SpinnerRod.TransX = sin( (sw33.CurrentAngle+180) * (2*PI/360)) * 3.5
	SpinnerRod.TransZ = sin( (sw33.CurrentAngle- 90) * (2*PI/360)) * 3.5
End Sub


'***********************************************************
'****					SLING CODE						****
'***********************************************************

Dim LStep

Sub LeftSlingShot_Slingshot
	RandomSoundSlingshotLeft
	DOF 121, DOFPulse
	LSling.Visible = 0
	LSling001.Visible = 1
	SlingL.Rotx = 22
	LStep = 0
	vpmTimer.PulseSw 50
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 1:	LSLing001.Visible = 0
				LSLing002.Visible = 1
				Slingl.Rotx = 9
		Case 2:	LSLing002.Visible = 0
				LSLing003.Visible = 1
				SlingL.Rotx = 0
		Case 3:	LSLing003.Visible = 0
				LSLing.Visible = 1
				SlingL.Rotx = 0
				LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub sw50c_Hit(idx):vpmTimer.PulseSw 50 : End Sub 

'***********************************************************
'****					BUMPER CODE						****
'***********************************************************
dim bump1step, bump2step, bump3step

Sub bumper1_Hit
	vpmTimer.PulseSw 43
	RandomSoundBumperBottom Bumper1
	layer1bumpskirt1.RotY=-3
	Bump1Step = 0
	bumper1.timerenabled = 1
End Sub

Sub bumper1_timer
	Select Case Bump1Step
		Case 3:	layer1bumpskirt1.RotY=-1
		Case 4: layer1bumpskirt1.RotY=1
		Case 5: layer1bumpskirt1.RotY=-1
		Case 6: layer1bumpskirt1.RotY=1
		Case 7: layer1bumpskirt1.RotY=0:bumper1.timerenabled = 0
	End Select
	Bump1Step=Bump1Step + 1
End Sub

Sub bumper2_Hit
	vpmTimer.PulseSw 53
	DOF 101, DOFPulse
	RandomSoundBumperTop Bumper2
	layer1bumpskirt2.RotY=-3
	Bump2Step = 0
	bumper2.timerenabled = 1
End Sub

Sub bumper2_timer
	Select Case Bump2Step
		Case 3:	layer1bumpskirt2.RotY=-1
		Case 4: layer1bumpskirt2.RotY=1
		Case 5: layer1bumpskirt2.RotY=-1
		Case 6: layer1bumpskirt2.RotY=1
		Case 7: layer1bumpskirt2.RotY=0:bumper2.timerenabled = 0
	End Select
	Bump2Step=Bump2Step + 1
End Sub

Sub bumper3_Hit
	vpmTimer.PulseSw 53
	DOF 102, DOFPulse
	RandomSoundBumperMiddle Bumper3
	layer1bumpskirt3.RotY=-3
	Bump3Step = 0
	bumper3.timerenabled = 1
End Sub

Sub bumper3_timer
	Select Case Bump3Step
		Case 3:	layer1bumpskirt3.RotY=-1
		Case 4: layer1bumpskirt3.RotY=1
		Case 5: layer1bumpskirt3.RotY=-1
		Case 6: layer1bumpskirt3.RotY=1
		Case 7: layer1bumpskirt3.RotY=0:bumper3.timerenabled = 0
	End Select
	Bump3Step=Bump3Step + 1
End Sub

'******************************************************************************
'Animated Rubbers Code

dim cstep, t10step, t8step

Sub RubberBand_17_Hit
	crubber.Visible = 0
	crubber001.Visible = 1
	cStep = 0
	crubbertime.Enabled = 1
End Sub

Sub crubbertime_Timer
	Select Case cStep
		Case 1:	crubber001.Visible = 0
				crubber002.Visible = 1
		Case 2:	crubber002.Visible = 0
				crubber003.Visible = 1
		Case 3:	crubber003.Visible = 0
				crubber.Visible = 1
				crubbertime.Enabled = 0
	End Select
	cStep = cStep + 1
End Sub

Sub RubberBand_8_Hit
	trubber.Visible = 0
	trubber8_001.Visible = 1
	t8Step = 0
	trubber8time.Enabled = 1
End Sub

Sub trubber8time_Timer
	Select Case t8Step
		Case 1:	trubber8_001.Visible = 0
				trubber8_002.Visible = 1
		Case 2:	trubber8_002.Visible = 0
				trubber8_003.Visible = 1
		Case 3:	trubber8_003.Visible = 0
				trubber.Visible = 1
				trubber8time.Enabled = 0
	End Select
	t8Step = t8Step + 1
End Sub

Sub RubberBand_10_Hit
	trubber.Visible = 0
	trubber10_001.Visible = 1
	t10Step = 0
	trubber10time.Enabled = 1
End Sub

Sub trubber10time_Timer
	Select Case t10Step
		Case 1:	trubber10_001.Visible = 0
				trubber10_002.Visible = 1
		Case 2:	trubber10_002.Visible = 0
				trubber10_003.Visible = 1
		Case 3:	trubber10_003.Visible = 0
				trubber.Visible = 1
				trubber10time.Enabled = 0
	End Select
	t10Step = t10Step + 1
End Sub

'******************************************************************************
'Primitive Flipper Code
Sub FlipperTimer_Timer
	LFlipb.rotz = LeftFlipper.currentangle
	LFlipr.rotz = LeftFlipper.currentangle
	LFlipb1.rotz = LeftFlipper1.currentangle
	LFlipr1.rotz = LeftFlipper1.currentangle
	RFlipb.rotz = RightFlipper.currentangle
	RFlipr.rotz = RightFlipper.currentangle
	RFlipb1.rotz = RightFlipper1.currentangle
	RFlipr1.rotz = RightFlipper1.currentangle
	RFlipb2.rotz = RightFlipper2.currentangle
	RFlipr2.rotz = RightFlipper2.currentangle

	batleftshadow.rotz = LeftFlipper.CurrentAngle
	batleftshadow1.rotz = LeftFlipper1.CurrentAngle
	batrightshadow.rotz  = RightFlipper.CurrentAngle	
	batrightshadow1.rotz  = RightFlipper1.CurrentAngle
	batrightshadow2.rotz  = RightFlipper2.CurrentAngle

	Pgate.rotx=-Gate.currentangle*.5
	plungegate_prim.RotX = Gate1.CurrentAngle + 70

	If plungerpress = 1 then
		If VR_Primary_plunger.Y < 2500 then
			VR_Primary_plunger.Y = VR_Primary_plunger.Y + 2.25
		End If
	Else
		VR_Primary_plunger.Y = 2390 + (5* Plunger.Position) -20
	End If

	GameTimer
End Sub
'******************************************************************************

'**********************************************************************************************************
 
'Map lights to an array
'**********************************************************************************************************

	Lights(4) = Array(l4p,l4pa) ' Shoot Again Playfied and backglass
Set Lights(5) = l5
	Lights(6) = Array(l6,L6a)
	Lights(7) = Array(l7,L7a)
	Lights(8) = Array(l8,L8a)
	Lights(9) = Array(l9,L9a)
	Lights(10) = Array(l10,L10a)
	Lights(11) = Array(l11,L11a)
Set Lights(12) = l12
	Lights(13) = Array(l13,L13a)
	Lights(14) = Array(l14,L14a)
	Lights(15) = Array(l15,L15a)
	Lights(16) = Array(l16,L16a)
	Lights(17) = Array(l17,L17a)
Set Lights(18) = l18
	Lights(19) = Array(l19,L19a)
	Lights(20) = Array(l20,L20a)
	Lights(21) = Array(l21,L21a)
	Lights(22) = Array(l22,L22a)
Set	Lights(23) = l23
Set Lights(24) = l24
Set Lights(25) = l25
Set Lights(26) = l26
Set Lights(27) = l27
Set Lights(28) = l28
Set Lights(29) = l29
Set Lights(30) = l30
Set Lights(31) = l31
	Lights(32) = Array(l32,l32a)
	Lights(33) = Array(l33,l33a)
	Lights(36) = Array(l36,L36a)
Set Lights(150) = Light1 'Star Trigger 1
Set Lights(151) = Light2 'Star Trigger 2
Set Lights(152) = Light3 'Star Trigger 3
Set Lights(153) = Light4 'Star Trigger 4
Set Lights(154) = Light5 'Star Trigger 5


'Backglass
Set Lights(1) = l1 'Game Over/Game On (BIP/Match)
Set Lights(2) = l2 'Tilt
Set Lights(3) = l3 'High Score
'**********************************************************************************************************
' Backglass Light Displays (7 digit 7 segment displays)
Dim Digits(32)
Digits(0)=Array(a00,a01,a02,a03,a04,a05,a06,n,a08)
Digits(1)=Array(a10,a11,a12,a13,a14,a15,a16,n,a18)
Digits(2)=Array(a20,a21,a22,a23,a24,a25,a26,n,a28)
Digits(3)=Array(a30,a31,a32,a33,a34,a35,a36,n,a38)
Digits(4)=Array(a40,a41,a42,a43,a44,a45,a46,n,a48)
Digits(5)=Array(a50,a51,a52,a53,a54,a55,a56,n,a58)
Digits(6)=Array(b00,b01,b02,b03,b04,b05,b06,n,b08)
Digits(7)=Array(b10,b11,b12,b13,b14,b15,b16,n,b18)
Digits(8)=Array(b20,b21,b22,b23,b24,b25,b26,n,b28)
Digits(9)=Array(b30,b31,b32,b33,b34,b35,b36,n,b38)
Digits(10)=Array(b40,b41,b42,b43,b44,b45,b46,n,b48)
Digits(11)=Array(b50,b51,b52,b53,b54,b55,b56,n,b58)
Digits(12)=Array(c00,c01,c02,c03,c04,c05,c06,n,c08)
Digits(13)=Array(c10,c11,c12,c13,c14,c15,c16,n,c18)
Digits(14)=Array(c20,c21,c22,c23,c24,c25,c26,n,c28)
Digits(15)=Array(c30,c31,c32,c33,c34,c35,c36,n,c38)
Digits(16)=Array(c40,c41,c42,c43,c44,c45,c46,n,c48)
Digits(17)=Array(c50,c51,c52,c53,c54,c55,c56,n,c58)
Digits(18)=Array(d00,d01,d02,d03,d04,d05,d06,n,d08)
Digits(19)=Array(d10,d11,d12,d13,d14,d15,d16,n,d18)
Digits(20)=Array(d20,d21,d22,d23,d24,d25,d26,n,d28)
Digits(21)=Array(d30,d31,d32,d33,d34,d35,d36,n,d38)
Digits(22)=Array(d40,d41,d42,d43,d44,d45,d46,n,d48)
Digits(23)=Array(d50,d51,d52,d53,d54,d55,d56,n,d58)
Digits(24)=Array(e00,e01,e02,e03,e04,e05,e06,n,e08)
Digits(25)=Array(e10,e11,e12,e13,e14,e15,e16,n,e18)
Digits(26)=Array(f00,f01,f02,f03,f04,f05,f06,n,f08)
Digits(27)=Array(f10,f11,f12,f13,f14,f15,f16,n,f18)


'**********************************************************************************************************
'Hanz's FSS EM Reel & Drum manager/interpreter script start
'**********************************************************************************************************

'Digital LED Display

Dim LEDFSS(28)
LEDFSS(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6, n101, LED1x8)
LEDFSS(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6, n101, LED2x8)
LEDFSS(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6, n101, LED3x8)
LEDFSS(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6, n101, LED4x8)
LEDFSS(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6, n101, LED5x8)
LEDFSS(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6, n101, LED6x8)
'LEDFSS(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6, n101, LED7x8)

LEDFSS(6) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6, n101, LED8x8)
LEDFSS(7) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6, n101, LED9x8)
LEDFSS(8) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6, n101, LED10x8)
LEDFSS(9) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6, n101, LED11x8)
LEDFSS(10) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6, n101, LED12x8)
LEDFSS(11) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6, n101, LED13x8)
'LEDFSS(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6, n101, LED14x8)

LEDFSS(12) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006, n101, LED1x008)
LEDFSS(13) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106, n101, LED1x108)
LEDFSS(14) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206, n101, LED1x208)
LEDFSS(15) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306, n101, LED1x308)
LEDFSS(16) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406, n101, LED1x408)
LEDFSS(17) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506, n101, LED1x508)
'LEDFSS(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606, n101, LED1x608)

LEDFSS(18) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006, n101, LED2x008)
LEDFSS(19) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106, n101, LED2x108)
LEDFSS(20) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206, n101, LED2x208)
LEDFSS(21) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306, n101, LED2x308)
LEDFSS(22) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406, n101, LED2x408)
LEDFSS(23) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506, n101, LED2x508)
'LEDFSS(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606, n101, LED2x608)

LEDFSS(26) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306, n101, LEDax308)
LEDFSS(27) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406, n101, LEDbx408)

LEDFSS(24) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506, n101, LEDcx508)
LEDFSS(25) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606, n101, LEDdx608)


Dim BGGB, BGGT, BGEB1,BGET1, BGNB, BGNT, BGI, BGEB2, BGET2, BGTime, BGDiff
Dim ShowFSS

ShowFSS = Table1.ShowFSS


Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		If VRRoom = 1 or ShowFSS or DesktopMode = False Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 28) then
					For Each obj In LEDFSS(num)
						If chg And 1 Then obj.visible = stat And 1 
						chg = chg\2 : stat = stat\2
					Next
				end if
			next
		Elseif DesktopMode Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 28) then
					For Each obj In Digits(num)
						If chg And 1 Then obj.State = stat And 1 
						chg = chg\2 : stat = stat\2
					Next
				end if
			next
		End If
	end if ' end if ChgLED

	If Bootcount = 2 Then
		Backglass_Tilt.visible = l2.state 'tilt
		Backglass_BIP.visible = l1.state 'BIP
		If l1.state Then 
			Backglass_Match.visible = false 
			Backglass_GameOver.visible = false
		else 
			Backglass_Match.visible = true 'Match
			Backglass_GameOver.visible = true 'game over
		End If
		Backglass_HighScore.visible = l3.state 'highscore
		Backglass_ShootAgain.visible = l4pa.state 'shoot again

		'Genie Lights
		BGDiff = Gametime - BGTime
		BGTime = Gametime

		BGGB = UpdateGenie(Backglass_GB, BGGB, BGDiff, 1)
		BGGT = UpdateGenie(Backglass_GT, BGGT, BGDiff, 0)
		BGEB1 = UpdateGenie(Backglass_EB1, BGEB1, BGDiff, 2)
		BGET1 = UpdateGenie(Backglass_ET1, BGET1, BGDiff, 0)
		BGNB = UpdateGenie(Backglass_NB, BGNB, BGDiff, 0)
		BGNT = UpdateGenie(Backglass_NT, BGNT, BGDiff, 1)
		BGI = UpdateGenie(Backglass_I, BGI, BGDiff, 0)
		BGEB2 = UpdateGenie(Backglass_EB2, BGEB2, BGDiff, 2)
		BGET2 = UpdateGenie(Backglass_ET2, BGET2, BGDiff, 0)	
	End If

End Sub

Function f455(fon,fspeed)
	'fon - 1 = turn on, 0 = turn off
	'fspeed - 0 = normal, 1 = medium, 2 = fast
	If fon = 0 then
		select case  fspeed
			case 0: f455 = rnd*100 + 900
			case 1: f455 = rnd*100 + 600
			case 2: f455 = rnd*100 + 150
		end select
	else
		select case  fspeed
			case 0: f455 = rnd*50 + 500
			case 1: f455 = rnd*50 + 350
			case 2: f455 = rnd*50 + 100
		end select
	end if
End Function

Function UpdateGenie(obj, bgtimer, delta, fspeed)
	bgtimer = bgtimer - delta
	if bgtimer <= 0 Then
		If obj.visible = 0 Then
			obj.visible = true
			UpdateGenie = F455(1,0)
		else
			obj.blenddisablelighting = 0.9
			UpdateGenie = F455(0,fspeed)
		end if
	else 
		UpdateGenie = bgtimer
	end if

	If obj.visible and obj.blenddisablelighting <> 1 then
		If obj.blenddisablelighting = 0 then
			obj.blenddisablelighting = 0.5
		Elseif obj.blenddisablelighting = 0.5 then
			obj.blenddisablelighting = 1
		Elseif obj.blenddisablelighting = 0.9 then
			obj.blenddisablelighting = 0.66
		Elseif obj.blenddisablelighting = 0.66 then
			obj.blenddisablelighting = 0.33
		Elseif obj.blenddisablelighting = 0.33 then
			obj.blenddisablelighting = 0
			obj.visible = false
		end if
	End If
End Function

dim xxx

Dim BootCount:BootCount = 0

Sub BootTable_Timer()
	If BootCount = 0 Then
		BootCount = 1
		PlaySoundAt "poweron", Plunger
		BootTable.interval = 500
	Else
		'*****GI Lights On
		dim xx
		For each xx in GI:xx.State = 1: Next
		for each xx in dtShadows: xx.visible=1: Next
		For each xx in layer1GI: xx.image = "layer1": Next
		For each xx in layer2GI: xx.image = "layer2": Next
		For each xx in layer3GI: xx.image = "layer3": Next
		bumpercap12.image="bumpercaps12"
		bumpercap3.image="bumpercap3"
		plasticlow.image="lowplastic"
		bumpbase.image="bumpbaseon"
		playfield_off.visible=0
		Backglass_GI.image = "Backglass_On"

		'Genie Lights
		BGGB = F455(1,0)
		BGGT = F455(0,0) - rnd*200
		BGEB1 = F455(0,0) - rnd*500
		BGET1 = F455(1,0)
		BGNB = F455(0,0) - rnd*200
		BGNT = F455(0,0) - rnd*500
		BGI = F455(1,0)
		BGEB2 = F455(0,0) - rnd*200
		BGET2 = F455(0,0) - rnd*500
		BGTime = Gametime
		BootCount = 2

		me.enabled = false
	End If
End Sub

'**********************************************************************************************************
 Sub editDips
 	Dim vpmDips : Set vpmDips = New cvpmDips
  	With vpmDips
  		.AddForm 700,400,"System 1 (Multi-Mode sound) - DIP switches"
  		.AddFrame 0,0,190,"Coin chute control",&H00040000,Array("seperate",0,"same",&H00040000)'dip 19
  		.AddFrame 0,46,190,"Game mode",&H00000400,Array("extra ball",0,"replay",&H00000400)'dip 11
  		.AddFrame 0,92,190,"High game to date awards",&H00200000,Array("no award",0,"3 replays",&H00200000)'dip 22
  		.AddFrame 0,138,190,"Balls per game",&H00000100,Array("5 balls",0,"3 balls",&H00000100)'dip 9
  		.AddFrame 0,184,190,"Tilt effect",&H00000800,Array("game over",0,"ball in play only",&H00000800)'dip 12
  		.AddFrame 205,0,190,"Maximum credits",&H00030000,Array("5 credits",0,"8 credits",&H00020000,"10 credits",&H00010000,"15 credits",&H00030000)'dip 17&18
  		.AddFrame 205,76,190,"Sound settings",&H80000000,Array("sounds",0,"tones",&H80000000)'dip 32
  		.AddFrame 205,122,190,"Attract tune",&H10000000,Array("no attract tune",0,"attract tune played every 6 minutes",&H10000000)'dip 29
  		.AddChk 205,175,190,Array("Match feature",&H00000200)'dip 10
  		.AddChk 205,190,190,Array("Credits displayed",&H00001000)'dip 13
  		.AddChk 205,205,190,Array("Play credit button tune",&H00002000)'dip 14
  		.AddChk 205,220,190,Array("Play tones when scoring",&H00080000)'dip 20
  		.AddChk 205,235,190,Array("Play coin switch tune",&H00400000)'dip 23
  		.AddChk 205,250,190,Array("High game to date displayed",&H00100000)'dip 21
  		.AddLabel 50,280,300,20,"After hitting OK, press F3 to reset game with new settings."
  		.ViewDips
  	End With
 End Sub
 Set vpmShowDips = GetRef("editDips")

'******************************************************
'						FUNCTIONS
'******************************************************

'*** PI returns the value for PI
Function PI()
	PI = 4*Atn(1)
End Function

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

'******************************************************
'       		RealTime Updates
'******************************************************

Dim BallShadow, DropCount
BallShadow = Array (BallShadow1)

Const tnob = 1 ' total number of balls
ReDim rolling(tnob)

Sub GameTimer()
	cor.update

	'*****************************************
	'	Ball Rolling Sounds
	'*****************************************

	If BallVel(GenieBall ) > 1 AND GenieBall.z < 30 Then
		rolling(0) = True
		PlaySound ("BallRoll_0"), -1, VolPlayfieldRoll(GenieBall) * 1.1 * VolumeDial, AudioPan(GenieBall), 0, PitchPlayfieldRoll(GenieBall), 1, 0, AudioFade(GenieBall)

	Else
		If rolling(0) = True Then
			StopSound("BallRoll_0")
			rolling(0) = False
		End If
	End If

	'*****************************************
	'	Ball Shadow
	'*****************************************
	
	BallShadow(0).X = GenieBall.X - (TableWidth/2 - GenieBall.X)/20
	ballShadow(0).Y = GenieBall.Y + 10

	If GenieBall.Z > 22 and GenieBall.Z < 35 Then
		BallShadow(0).visible = 1
	Else
		BallShadow(0).visible = 0
	End If

	'*****************************************
	'	Ball Drop Sounds
	'*****************************************

	If GenieBall.VelZ < -1 and GenieBall.z < 55 and GenieBall.z > 27 Then 'height adjust for ball drop sounds
		If DropCount >= 5 Then
			RandomSoundBallBouncePlayfieldSoft
			DropCount = 0
		End If
	End If
	If DropCount < 5 Then
		DropCount = DropCount + 1
	End If

End Sub

'******************************************************
'			FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks LeftFlipper1, LFPress1, LFCount1, LFEndAngle1, LFState1
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperTricks RightFlipper1, RFPress1, RFCount1, RFEndAngle1, RFState1
	FlipperTricks RightFlipper2, RFPress2, RFCount2, RFEndAngle2, RFState2
end sub

dim LFPress, RFPress, RFPress1, RFPress2, LFPress1
dim LFState, RFState, LFState1, RFState1, RFState2
dim EOST, EOSA, Frampup, FElasticity, FReturn, FReturn1
dim RFEndAngle, RFEndAngle1, RFEndAngle2, LFEndAngle, LFEndAngle1
dim LFCount, LFCount1, RFCount, RFCount1, RFCount2

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1.2 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.055

LFEndAngle = Leftflipper.endangle
LFEndAngle1 = Leftflipper1.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle1 = RightFlipper1.endangle
RFEndAngle2 = RightFlipper2.endangle

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
Dim LiveDistanceMax: LiveDistanceMax = leftflipper.length  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

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

Sub LeftFlipper1_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper1, LFCount1, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper1, RFCount1, parm
	RightFlipperCollide parm
End Sub

Sub RightFlipper2_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper2, RFCount2, parm
	RightFlipperCollide parm
End Sub


'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
dim RF1 : Set RF1 = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF, RF1)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 80
	Next

	'rf.report "Polarity"

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -2.7        
	AddPt "Polarity", 2, 0.33, -2.7
	AddPt "Polarity", 3, 0.37, -2.7        
	AddPt "Polarity", 4, 0.41, -2.7
	AddPt "Polarity", 5, 0.45, -2.7
	AddPt "Polarity", 6, 0.576,-2.7
	AddPt "Polarity", 7, 0.66, -1.8
	AddPt "Polarity", 8, 0.743, -0.5
	AddPt "Polarity", 9, 0.81, -0.5
	AddPt "Polarity", 10, 0.88, 0

	'"Velocity" Profile
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
	RF1.Object = RightFlipper1
	RF1.EndPoint = EndPointRp1
End Sub

'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
Sub TriggerRF1_Hit() : RF1.Addball activeball : End Sub
Sub TriggerRF1_UnHit() : RF1.PolarityCorrect activeball : End Sub

'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF, RF1)
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
				'playsound "knocker_1"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
'******************************************************

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

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub


Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

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


Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.11 '0.97'	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.99 '0.97 '0.935 '0.96
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
FlippersD.addpoint 0, 0, 1.11	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.97

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
				aball.vely = aball.vely * (1.1 + (parm/50))
			Elseif parm <= 2 and parm > 0.2 And aball.vely < 0 Then
				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        	aball.angmomz = aball.angmomz * -0.7
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				aball.vely = aball.vely * (1.2 + (parm/10))
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
	public ballvel, ballvelx, ballvely, ballangmomz

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : redim ballangmomz(0) End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds
		if uBound(ballangmomz) < highestID then redim ballangmomz(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
			ballangmomz(b.id) = b.angmomz
		Next
	End Sub
End Class


'******************************************************
' 					VRROOM SETUP
'******************************************************

Dim VRRoomSel, Object

VRRoomSel = LoadValue("Genie", "V1.0.0")  ' loading the last value saved.  These are saved below each time you make a change with magnasave

Sub RoomSphere()
	VRAllOff
	for each Object in VR_Sphere : object.visible = 1 : next
	VR_LegBL.z=100
	VR_LegBR.z=100
	VR_Backbox.blenddisablelighting = 0.3
	VR_Cabinet.blenddisablelighting = 0.3
	SaveValue "Genie", "V1.0.0", 0
End Sub

Sub RoomMinimal()
	VRAllOff
	for each Object in VR_Minimal : object.visible = 1 : next
	VR_LegBL.z=100
	VR_LegBR.z=100
	VR_Backbox.blenddisablelighting = 0.2
	VR_Cabinet.blenddisablelighting = 0.2
	SaveValue "Genie", "V1.0.0", 1
End Sub

Sub RoomTotan()
	VRAllOff
	for each Object in VR_Totan : object.visible = 1 : next
	VR_LegBL.z=70
	VR_LegBR.z=70
	VR_Backbox.blenddisablelighting = 0.1
	VR_Cabinet.blenddisablelighting = 0.1
	SaveValue "Genie", "V1.0.0", 2
End Sub

Sub VRAllOff()
	for each Object in VR_Sphere : object.visible = 0 : next
	for each Object in VR_Minimal : object.visible = 0 : next
	for each Object in VR_Totan : object.visible = 0 : next	
End Sub

Sub VRChangeRoom()
	If VRRoomSel = 0 Then  ' If this is the first run of the table, there will be no value saved, we default it to 1 here..
		RoomSphere
	Elseif VRRoomSel = 1 Then
		RoomMinimal
	Elseif VRRoomSel = 1 Then
		RoomMinimal
	Elseif VRRoomSel = "" or VRRoomSel = 2 Then
		VRRoomSel = 2
		RoomTotan		 
	End If
End Sub

'******************************************************
'				BACKGLASS SETUP
'******************************************************

Dim yoff,zoff, xrot

yoff = 112
zoff = 132
xrot = -90

Sub set_Backglass()
	Dim ii,yy,obj

	for ii =0 to 27
		For Each obj In LEDFSS(ii)
			yy = obj.y ' get the yoffset before it is changed
			obj.y =yoff 

			If(yy < 0.) then
				yy = yy * -1
			end if

			obj.height =( zoff) + yy
			
			obj.rotx = xrot
		Next
	Next

	For each obj in LEDsOff
		yy = obj.y ' get the yoffset before it is changed
		obj.y = yoff - 0.1

		If(yy < 0.) then
			yy = yy * -1
		end if

		obj.height =( zoff) + yy
		
		obj.rotx = xrot
	Next
	
	For each obj in Backglass
		obj.rotx = -90
		obj.y = 115
		obj.z = 608 + zoff
	Next

	For each obj in Backglass
		If obj.blenddisablelighting > 0.8 Then
			obj.blenddisablelighting = 15
		Elseif obj.blenddisablelighting > 0.6 Then
			obj.blenddisablelighting = 0
		Else
			obj.blenddisablelighting = 0.5
		End If
	Next

end sub

set_Backglass

'***************** CODE BELOW IS FOR THE VR CLOCK.  This was originally taken from Rascal VP9 clock table *******************************
'*****************************************************************************************************************************************

Dim CurrentMinute ' for VR clock

Sub VR_ClockTimer_Timer()

    'ClockHands Below *********************************************************
	VR_Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	VR_Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    VR_Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())

End Sub


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
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

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
	PlaySoundAtLevelStatic ("Start_Button"), StartButtonSoundLevel, Drain
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
		Case 1 : PlaySoundAtLevelStatic ("Nudge_1"), NudgeCenterSoundLevel * VolumeDial, Drain
		Case 2 : PlaySoundAtLevelStatic ("Nudge_2"), NudgeCenterSoundLevel * VolumeDial, Drain
		Case 3 : PlaySoundAtLevelStatic ("Nudge_3"), NudgeCenterSoundLevel * VolumeDial, Drain
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
Sub RandomSoundSlingshotLeft()
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_L1",DOFContactors), SlingshotSoundLevel, SlingL
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_L2",DOFContactors), SlingshotSoundLevel, SlingL
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_L3",DOFContactors), SlingshotSoundLevel, SlingL
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_L4",DOFContactors), SlingshotSoundLevel, SlingL
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_L5",DOFContactors), SlingshotSoundLevel, SlingL
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_L6",DOFContactors), SlingshotSoundLevel, SlingL
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_L7",DOFContactors), SlingshotSoundLevel, SlingL
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_L8",DOFContactors), SlingshotSoundLevel, SlingL
		Case 9 : PlaySoundAtLevelStatic SoundFX("Sling_L9",DOFContactors), SlingshotSoundLevel, SlingL
		Case 10 : PlaySoundAtLevelStatic SoundFX("Sling_L10",DOFContactors), SlingshotSoundLevel, SlingL
	End Select
End Sub

Sub RandomSoundSlingshotRight()
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_R1",DOFContactors), SlingshotSoundLevel, Sling1
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_R2",DOFContactors), SlingshotSoundLevel, Sling1
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_R3",DOFContactors), SlingshotSoundLevel, Sling1
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_R4",DOFContactors), SlingshotSoundLevel, Sling1
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_R5",DOFContactors), SlingshotSoundLevel, Sling1
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_R6",DOFContactors), SlingshotSoundLevel, Sling1
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_R7",DOFContactors), SlingshotSoundLevel, Sling1
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_R8",DOFContactors), SlingshotSoundLevel, Sling1
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

Sub aPostRubbers_Hit(idx)
	Rubbers_Hit idx	
End Sub

Sub aRubbers_Hit(idx)
	Rubbers_Hit idx	
End Sub

Sub RubberWheel_Hit(idx)
 	'RandomSoundRubberWeakWheel
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

Sub RandomSoundRubberWeakWheel()
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor/10
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor/10
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

Sub aWoods_Hit(idx)
	Walls_Hit idx
End Sub

Sub aPlastics_Hit(idx)
	Walls_Hit idx
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

Sub aMetals_Hit (idx)
	RandomSoundMetal
End Sub

Sub DiverterFlipper_collide(idx)
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


Sub aApron_Hit (idx)
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
		RandomSoundBallBouncePlayfieldSoft()
	Else 
 		RandomSoundTargetHitWeak()
 	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft()
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(GenieBall) * BallBouncePlayfieldSoftFactor, GenieBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(GenieBall) * BallBouncePlayfieldSoftFactor * 0.5, GenieBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(GenieBall) * BallBouncePlayfieldSoftFactor * 0.8, GenieBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(GenieBall) * BallBouncePlayfieldSoftFactor * 0.5, GenieBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(GenieBall) * BallBouncePlayfieldSoftFactor, GenieBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(GenieBall) * BallBouncePlayfieldSoftFactor * 0.2, GenieBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(GenieBall) * BallBouncePlayfieldSoftFactor * 0.2, GenieBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(GenieBall) * BallBouncePlayfieldSoftFactor * 0.2, GenieBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(GenieBall) * BallBouncePlayfieldSoftFactor * 0.3, GenieBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard()
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(GenieBall) * BallBouncePlayfieldHardFactor, GenieBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(GenieBall) * BallBouncePlayfieldHardFactor, GenieBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_3"), volz(GenieBall) * BallBouncePlayfieldHardFactor, GenieBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_4"), volz(GenieBall) * BallBouncePlayfieldHardFactor, GenieBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(GenieBall) * BallBouncePlayfieldHardFactor, GenieBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_6"), volz(GenieBall) * BallBouncePlayfieldHardFactor, GenieBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(GenieBall) * BallBouncePlayfieldHardFactor, GenieBall
	End Select
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, GenieBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, GenieBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, GenieBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, GenieBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, GenieBall
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

Sub aGates_hit(idx)
	SoundHeavyGate
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
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -6.5 and activeball.vely < -2 Then
		RandomSoundRightArch
	End If	
End Sub

Dim Arch4Dir

Sub Arch4_hit()
	If Activeball.velx < -1 Then SoundPlayfieldGate: Arch4Dir = 0: Else Arch4Dir = 1
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch4_unhit()
	'debug.print "Arch4u: " & activeball.velx & " " & activeball.vely
	If activeball.velx > 12  and activeball.vely < -2  and Arch4Dir = 0 Then
		RandomSoundLeftArch
	Elseif activeball.velx > 10  and activeball.vely < -1  and Arch4Dir = 1 Then
		RandomSoundLeftArch
	End If
End Sub

Sub Arch3_hit()
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch3_unhit()
	If activeball.vely < -10 Then
		RandomSoundRightArch
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


