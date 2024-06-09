Option Explicit
Randomize

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Player Options
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	dim toppervideo:toppervideo = 1 'set to 1 to turn on the topper
	dim noscores: noscores = 0 'set to 1 to take the scores off the backglass
	dim videovolume:videovolume = 90 ' adjust volumn of video track here
	dim calloutvolume:calloutvolume = 90 ' adjust the volumn of the callouts here
	' of note the table audio isn't adjustable sooo adjust the callouts and videos as needed to make it right for you.

' additional options
Const HarderModes = False        	'Mode feature must be collected at least once to complete a Mode  
Const BM_Multiball_Level = 1		'Set the level at which Beatles Mania will be played as an untimed 3-Ball Multiball, >4 is OFF
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="seawitfp",UseSolenoids=1,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SCoin="coin"

LoadVPM "01560000", "stern.vbs", 3.26
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
	Ramp16.visible=1
	Ramp15.visible=1
Else
	Ramp16.visible=0
	Ramp15.visible=0
End if

'**********************************************************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(6) =  "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(7) = "dtBank.SolDropUp" 'Drop Targets
SolCallback(8) = "dtLBank.SolDropUp" 'Drop Targets
SolCallback(9) = "dtRBank.SolDropUp" 'Drop Targets
SolCallback(10) = "SolBallRelease"

Dim PlayerNo, BallinPlay, c, cc, ShootAgain, BallsSaved
BallinPlay=0:ShootAgain=False
Sub SolBallRelease(enabled)
	If enabled Then
		'Start Ballsaver only once per Ball
		BallsSaved = 0
		bspos = 1
		addtllightsoff
		ballsave.state = 2

		bsTrough.ExitSol_On
		if ballinplay <> scores(31) then
			ballinplay = scores(31)
			if Ballinplay = 1 Then			'new game started
				for c = 1 to 4
					for cc = 1 to 6 
						Modecompleted(c,cc) = False
					next	
					Modelevel(c) = 1
					Jackpotvalue(c) = 25000
				Next
			end if
			if Playerno <> 1 Then
				Playerno = 1
			end if
		Else
			if not ShootAgain then
				Playerno = Playerno + 1
			Else
				if HasPup Then
					PuPlayer.LabelSet pBackglass,"BallSavedbg","Shoot Again!",1,""
					vpmtimer.addtimer 2000, "PuPlayer.LabelSet pBackglass,""BallSavedbg"","""",1,"""" '"
				end if
				'moved to StartSkillshot
				'ShootAgain=False				
			end If
		end if

		'Restore Mode status for active player
		Mode1Light.state = Lightstateoff
		Mode2Light.state = Lightstateoff	
		Mode3Light.state = Lightstateoff	
		Mode4Light.state = Lightstateoff	
		Mode5Light.state = Lightstateoff	
		Drum.state = Lightstateoff

		Mode1Light.color = Modecolor(Modelevel(PlayerNo))
		Mode1Light.ColorFull = Modecolor(Modelevel(PlayerNo))
		Mode2Light.Color = Modecolor(Modelevel(PlayerNo))
		Mode2Light.ColorFull = Modecolor(Modelevel(PlayerNo))
		Mode3Light.Color = Modecolor(Modelevel(PlayerNo))
		Mode3Light.ColorFull = Modecolor(Modelevel(PlayerNo))
		Mode4Light.Color = Modecolor(Modelevel(PlayerNo))
		Mode4Light.ColorFull = Modecolor(Modelevel(PlayerNo))
		Mode5Light.Color = Modecolor(Modelevel(PlayerNo))
		Mode5Light.ColorFull = Modecolor(Modelevel(PlayerNo))

		if Modecompleted(PlayerNo,1) then 
			Mode1Light.state = Lightstateon
		end If
		if Modecompleted(PlayerNo,2) then 
			Mode2Light.state = Lightstateon
		end If
		if Modecompleted(PlayerNo,3) then 
			Mode3Light.state = Lightstateon
		end If
		if Modecompleted(PlayerNo,4) then
			Mode4Light.state = Lightstateon
		end If
		if Modecompleted(PlayerNo,5) then 
			Mode5Light.state = Lightstateon
		end If
		if Modecompleted(PlayerNo,0) then
			Mode1Light.state = Lightstateon
			Mode2Light.state = Lightstateon
			Mode3Light.state = Lightstateon
			Mode4Light.state = Lightstateon
			Mode5Light.state = Lightstateon
			Drum.state = Lightstateon
		end If
		ModeReady = 1
		'Activate Beatlemania Mode
		if Modecompleted(PlayerNo,1) and Modecompleted(PlayerNo,2) and Modecompleted(PlayerNo,3) and Modecompleted(PlayerNo,4) and Modecompleted(PlayerNo,5) and (not Modecompleted(PlayerNo,0)) then
			ModeReady = 6
			Mode1Light.state = Lightstateblinking
			Mode2Light.state = Lightstateblinking
			Mode3Light.state = Lightstateblinking
			Mode4Light.state = Lightstateblinking
			Mode5Light.state = Lightstateblinking
		end If

		'passing the Skillshot will enable the next Mode
		NextMode
		ModeCycleTimer.enabled = True

		NoExploit = True
		NoExploitTimer.enabled = True	
	end if
End Sub


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
Dim rfup, lfup
Sub SolLFlipper(Enabled)
     If Enabled Then
		lfup=1
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
		lfup=0
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
		rfup=1
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
     Else
		rfup=0
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart:RightFlipper1.RotateToStart
     End If
End Sub
'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************


'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next


'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, dtBank, dtLBank, dtRBank

Sub Table1_Init
	'spinner.MotorOn = True
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "The Beatles"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.Games(cGameName).Settings.Value("sound")=0
        .hidden = 1
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run
	If Err Then MsgBox Err.Description
	If Err Then MsgBox Err.Description
	On Error Goto 0
	PupInit
	resetbackglass
          PinMAMETimer.Interval = PinMAMEInterval
          PinMAMETimer.Enabled = 1
          vpmNudge.TiltSwitch = 7
          vpmNudge.Sensitivity = 2
          vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3,LeftSlingShot, RightSlingShot)
 
          Set bsTrough = New cvpmBallStack
     		  bsTrough.InitNoTrough ballrelease, 33, 90, 4 
 			  bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
 
          set dtLBank = new cvpmdroptarget
              dtLBank.InitDrop Array(sw40, sw39, sw38), Array(40, 39, 38)
              dtLBank.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

          set dtRBank = new cvpmdroptarget
              dtRBank.InitDrop Array(sw32, sw31, sw30, sw29), Array(32, 31, 30, 29)
              dtRBank.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

          set dtBank = new cvpmdroptarget
              dtBank.InitDrop Array(sw21, sw22, sw23, sw24), Array(21, 22, 23, 24)
              dtBank.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)
 End Sub
 
'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	if keycode = startgamekey then ResetModes
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

'**********************************************************************************************************

'Drain hole
Sub Drain_Hit
	if Lampstate(11) Then
		ShootAgain=True
	End If

	playsound"drain"
	bsTrough.addball me
	Turntablecount = 0
	EndMystery
	ClearMysteryDisplay
	EndMode
End Sub

'Spinners
Sub sw5_Spin()
	if Spinnerbonus Then
		AddPoints 1000
	end if
	vpmTimer.PulseSw 5
	playsound"fx_spinner"
	PlaySound "girlscreams"
	FlashLevel2 = 1
	Flasherflash2_Timer
	CheckSuperSpinner
	CheckBeatlemania
End Sub

Sub leftspin_Spin()
	if Spinnerbonus Then
		AddPoints 1000
	end if
	vpmTimer.PulseSw 5
	playsound"fx_spinner"
	PlaySound "girlscreams"
	FlashLevel7 = 1
	Flasherflash7_Timer
	CheckSuperSpinner
	CheckBeatlemania
End Sub

'Bumpers
Sub Bumper1_Hit:bumperflashing:vpmTimer.PulseSw 9:playsound SoundFX("fx_bumper1",DOFContactors):SetLamp 181, 0:Me.TimerEnabled = 1:CheckSuperPopBumper:CheckBeatlemania:End Sub
    
Sub Bumper1_Timer()
SetLamp 181, 1
Me.TimerEnabled = 0
End Sub

Sub Bumper2_Hit:bumperflashing:vpmTimer.PulseSw 10:playsound SoundFX("fx_bumper1",DOFContactors):SetLamp 180, 0:Me.TimerEnabled = 1:CheckSuperPopBumper:CheckBeatlemania:End Sub

Sub Bumper2_Timer()
	SetLamp 180, 1
	Me.TimerEnabled = 0
End Sub

Sub Bumper3_Hit:bumperflashing:vpmTimer.PulseSw 14:playsound SoundFX("fx_bumper1",DOFContactors):SetLamp 182, 0:Me.TimerEnabled = 1:CheckSuperPopBumper:CheckBeatlemania:End Sub

Sub Bumper3_Timer()
	SetLamp 182, 1
	Me.TimerEnabled = 0
End Sub	


'Star Trigger
Sub sw4_Hit:Controller.Switch(4) = 1:PlaySound "rollover":PlaySound "big drum":AddTurntableTime:CheckBeatlemania:End Sub
Sub sw4_UnHit:Controller.Switch(4) = 0:End Sub
'Sub sw25_Hit()
''DivDir = -1
''closedSL.IsDropped=1
''OpenSL.IsDropped=0
'Controller.Switch(25) = 1
'lrro.State=0
'PlaySound "rollover"
'End Sub
'Sub sw25_UnHit():Controller.Switch(25) = 0:lrro.State=1:End Sub

 'Rollovers
Sub sw34_Hit::Controller.Switch(34) = 1:PlaySound "rollover":flashdrum:PlaySound "sitar":CheckBeatlemania:End Sub
Sub sw34_UnHit::Controller.Switch(34) = 0:End Sub
Sub sw35_Hit::Controller.Switch(35) = 1:PlaySound "rollover":flashdrum:PlaySound "sitar":CheckBeatlemania:End Sub
Sub sw35_UnHit::Controller.Switch(35) = 0:End Sub
Sub sw36_Hit::Controller.Switch(36) = 1:PlaySound "rollover":flashdrum:PlaySound "cometogether":CheckBeatlemania:End Sub
Sub sw36_UnHit::Controller.Switch(36) = 0:End Sub
Sub sw37_Hit::Controller.Switch(37) = 1:PlaySound "rollover":flashdrum:PlaySound "cometogether":CheckBeatlemania:End Sub
Sub sw37_UnHit::Controller.Switch(37) = 0:End Sub

'Standup Targets 
Sub sw17_Hit:vpmTimer.PulseSw 17:flashdrum:FlashLevel3 = 1 : Flasherflash3_Timer:CheckBeatlemania:End Sub
Sub sw18_Hit:vpmTimer.PulseSw 18:flashdrum:FlashLevel7 = 1 : Flasherflash7_Timer:CheckBeatlemania:End Sub
Sub sw19_Hit:vpmTimer.PulseSw 19:flashdrum:FlashLevel7 = 1 : Flasherflash7_Timer:CheckBeatlemania:End Sub
Sub sw20_Hit:vpmTimer.PulseSw 20:flashdrum:FlashLevel7 = 1 : Flasherflash7_Timer:CheckBeatlemania:End Sub

'Drop Targets
Sub Sw21_Dropped: DTBank.Hit 1:SetDTState 3,DTBank,fourf:flashdrum:PlaySound "chord":AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub 
Sub Sw22_Dropped: DTBank.Hit 2:SetDTState 4,DTBank,fouro:flashdrum:PlaySound "chord":AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub
Sub Sw23_Dropped: DTBank.Hit 3:SetDTState 5,DTBank,fouru:flashdrum:PlaySound "chord":AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub
Sub Sw24_Dropped: DTBank.Hit 4:SetDTState 6,DTBank,fourr:flashdrum:PlaySound "chord":AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub

Sub Sw40_hit: DTLBank.Hit 1:SetDTState 0,DTLBank,fabf:checkfab:flashdrum:PlaySound "rooster":AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub 
Sub Sw39_hit: DTLBank.Hit 2:SetDTState 1,DTLBank,faba:checkfab:flashdrum:PlaySound "rooster":AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub
Sub Sw38_hit: DTLBank.Hit 3:SetDTState 2,DTLBank,fabb:checkfab:flashdrum:PlaySound "rooster":AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub

Sub Sw32_Dropped: DTRBank.Hit 1:SetDTState 10,DTRBank,l19644:checkletters:flashdrum:PlaySound "big drum":FlashLevel3 = 1 : Flasherflash3_Timer:AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub 
Sub Sw31_Dropped: DTRBank.Hit 2:SetDTState 9,DTRBank,l19646:checkletters:flashdrum:PlaySound "big drum":FlashLevel3 = 1 : Flasherflash3_Timer:AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub
Sub Sw30_Dropped: DTRBank.Hit 3:SetDTState 8,DTRBank,l19649:checkletters:flashdrum:PlaySound "big drum":FlashLevel3 = 1 : Flasherflash3_Timer:AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub
Sub Sw29_Dropped: DTRBank.Hit 4:SetDTState 7,DTRBank,l19641:checkletters:flashdrum:PlaySound "big drum":FlashLevel3 = 1 : Flasherflash3_Timer:AddTurntableTime:CheckDTFrenzy:CheckBeatlemania:end sub

Dim Spinnerbonus
sub checkletters
	if l19644.state + l19646.state + l19649.state + l19641.state = 4 Then
		spinflashleft.visible = True
		spinflashright.visible = True
		Spinnerbonus = True
	end if
end Sub


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
LampTimer.Interval = 5 'lamp fading speed
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
End Sub

Sub UpdateLamps

        NFadeL 1, l1 
        NFadeLm 2, l2
        Flash 2, f2
        NFadeLm 3, l3
		Flash 3, f3
        NFadeLm 4, l4b
        NFadeL 4, l4
        NFadeL 5, l5
        NFadeL 6, l6
        NFadeL 7, l7
        NFadeL 8, l8
        NFadeL 9, l9
        NFadeL 10, l10
        NFadeL 11, l11
        NFadeL 12, l12
     '   FadeR 13, hs 'High Score
        NFadeL 14, l14
        NFadeL 15, l15
        NFadeLm 17, l17
		Flash 17, f17
        NFadeLm 18, l18
		Flash 18, f18
        NFadeLm 19, l19
		Flash 19, f19
        NFadeL 20, l20
        NFadeL 21, l21
        NFadeL 22, l22
        NFadeL 23, l23
        NFadeL 24, l24
        NFadeL 25, l25
        NFadeL 26, l26
        NFadeL 27, l27  
        NFadeL 28, l28 
        NFadeL 30, l30
        NFadeL 31, l31
        NFadeLm 33, l33
		Flash 33, f33
        NFadeLm 34, l34
		Flash 34, f34
        NFadeL 35, l35
        NFadeL 36, l36
        NFadeL 37, l37
        NFadeL 38, l38
        NFadeL 39, l39
        NFadeL 40, l40
        NFadeL 42, l42
        NFadeL 43, l43
        NFadeL 44, l44
     '   FadeR 45, go 'Game Over  
        NFadeL 46, l46
        NFadeL 47, l47
        NFadeLm 49, l49
        Flash 49, F49
        NFadeLm 50, l50
		Flash 50, f50 
 
        NFadeL 52, l52 
        NFadeL 53, l53 
        NFadeL 54, l54 
        NFadeL 55, l55 
        NFadeL 56, l56
        NFadeL 58, l58 
        NFadeL 59, l59
        NFadeL 60, l60 
  '      FadeR 61, tilt 'Tilt 
        NFadeL 62, l62 
   '     FadeR 63, match 'Match

		NFadeLm 180, fb3a1
		NFadeL 180, fb1
		NFadeLm 181, fb3a2
		NFadeL 181, fb2
		NFadeLm 182, fb3a
		NFadeL 182, fb3

	if haspup then
		if LampState(45) then
			PuPlayer.LabelSet pBackglass,"GameOverbg","Game Over",1,""
			GameOverTextDisplayed = True
			dsv = 0
		Else
			if GameOverTextDisplayed then
				PuPlayer.LabelSet pBackglass,"GameOverbg","",1,""
				GameOverTextDisplayed = False
			end If
		end If
	end If
		
	if (bspos = 0) and (Lampstate(11) <> ballsave.state) Then
		ballsave.state = Lampstate(11)
	End If

	if (not Skillshottimer.enabled) and (not Skillshotactive) then
		if ModeActiveTimer.enabled Then
			if lrro.state <> LightstateBlinking Then
				lrro.state = LightstateBlinking
			end if
		Else
			if lrro.state <> LightstateOn Then
				lrro.state = LightstateOn
			end if
		end If
	end if

	if BeatlemaniaMBTimer.enabled Then
		if lrro.state <> LightstateBlinking Then
			lrro.state = LightstateBlinking
		end if
	end if

	ModeLevel1LightA.State = cint(Modelevel(PlayerNo) > 1)
	ModeLevel1LightB.State = cint(Modelevel(PlayerNo) > 1)
	ModeLevel1LightC.State = cint(Modelevel(PlayerNo) > 1)
	ModeLevel2LightA.State = cint(Modelevel(PlayerNo) > 2)
	ModeLevel2LightB.State = cint(Modelevel(PlayerNo) > 2)
	ModeLevel3LightA.State = cint(Modelevel(PlayerNo) > 3)
	ModeLevel4LightA.State = cint(Modelevel(PlayerNo) > 4)
	ModeLevel4LightB.State = cint(Modelevel(PlayerNo) > 4)
	ModeLevel4LightC.State = cint(Modelevel(PlayerNo) > 4)
	ModeLevel4LightD.State = cint(Modelevel(PlayerNo) > 4)

	If ballinlock = 1 Then
		if BallLockLight.state <> Lightstateblinking Then
			BallLockLight.state = Lightstateblinking
		End If
	Else
		if BallLockLight.state <> Lightstateoff Then
			BallLockLight.state = Lightstateoff
		End If
	end If
End Sub

Dim GameOverTextDisplayed

' div lamp subs
Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
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
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
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


' Modulated Flasher and Lights objects
Sub SetLampMod(nr, value)
    If value > 0 Then
        LampState(nr) = 1
    Else
        LampState(nr) = 0
    End If
    FadingLevel(nr) = value
End Sub
 
Sub LampMod(nr, object)
    If TypeName(object) = "Light" Then
        Object.IntensityScale = FadingLevel(nr)/128
        Object.State = LampState(nr)
    End If
    If TypeName(object) = "Flasher" Then
        Object.IntensityScale = FadingLevel(nr)/128
        Object.visible = LampState(nr)
    End If
    If TypeName(object) = "Primitive" Then
        Object.DisableLighting = LampState(nr)
    End If
End Sub


 'Reels
Sub FadeReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 0:FadingLevel(nr) = 3
        Case 5:reel.Visible = 1:FadingLevel(nr) = 1
    End Select
End Sub

 'Inverted Reels
Sub FadeIReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 1:FadingLevel(nr) = 3
        Case 5:reel.Visible = 0:FadingLevel(nr) = 1
    End Select
End Sub
 

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
Dim Digits(32)

' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)

' 2nd Player
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)

' 3rd Player
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)

' 4th Player
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)

' Credits
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
' Balls
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)




'Stern Sea Witch
'added by Inkochnito
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Sea Witch - DIP switches"
		.AddChk 2,10,115,Array("Match feature",&H00100000)'dip 21
		.AddChk 120,10,115,Array("Credits display",&H00080000)'dip 20
		.AddChk 240,10,125,Array("Background sound",&H00002000)'dip 14
		.AddFrame 2,30,190,"Maximum credits",&H00060000,Array("10 credits",0,"15 credits",&H00020000,"25 credits",&H00040000,"40 credits",&H00060000)'dip 18&19
		.AddFrame 2,105,190,"High game to date",49152,Array("points",0,"1 free game",&H00004000,"2 free games",32768,"3 free games",49152)'dip 15&16
		.AddFrame 2,180,190,"High score feature",&H00000020,Array("extra ball",0,"replay",&H00000020)'dip 6
		.AddFrame 2,226,190,"Special adjustment",&H00000080,Array("special on 7X",0,"special on 6X or 7X and resets to 3X",&H00000080)'dip 8
		.AddFrame 2,272,190,"Outlane special",&H00010000,Array("alternating",0,"both lites stay on",&H00010000)'dip 17
		.AddFrame 205,30,190,"Special award",&HC0000000,Array("no award",0,"100,000 points",&H40000000,"free ball",&H80000000,"free game",&HC0000000)'dip 31&32
		.AddFrame 205,105,190,"Extra ball lites",&H00C00000,Array("completely off",0,"left on, both off, right on, both off",&H00400000,"alternate left to right",&H00800000,"alternate both on and off",&H00C00000)'dip 23&24
		.AddFrame 205,180,190,"Balls per game",&H00000040,Array("3 balls",0,"5 balls",&H00000040)'dip 7
		.AddFrame 205,226,190,"Special limit",&H00200000,Array("1 per ball",0,"1 per game",&H00200000)'dip 22
		.AddFrame 205,272,190,"Extra ball memory",&H30000000,Array ("only 1 stored in memory",0,"maximum 3 stored in memory",&H10000000,"maximum 5 stored in memory",&H30000000)'dip 29&30
		.AddLabel 50,340,300, 20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub
Set vpmShowDips = GetRef("editDips")

'******************************************************************************************************
'*******************************************************************************************************
'VPX Call Backs
'*******************************************************************************************************
'*******************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
PlaySound "snares"
FlashLevel1 = 1 : Flasherflash1_Timer
	vpmTimer.PulseSw 13
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	CheckBeatlemania
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
PlaySound "snares"
	FlashLevel5 = 1 : Flasherflash5_Timer
	vpmTimer.PulseSw 12
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	CheckBeatlemania
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub


'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function



'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

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
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperLSh1.RotZ = LeftFlipper1.currentangle
	FlipperRSh1.RotZ = RightFlipper1.currentangle
End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) '+ 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) '- 6
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub



'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Controller VBS stuff, but with b2s not started
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'***Controller.vbs version 1.2***'
'
'by arngrim
'
'This script was written to have a generic way to define a controller, no matter if the table is EM or SS based.
'It will also try to load the B2S.Server and if it is not present (or forced off),
'just the standard VPinMAME.Controller is loaded for SS generation games, or no controller for EM ones.
'
'At the first launch of a table using Controller.vbs, it will write into the registry with these default values
'ForceDisableB2S = 0
'DOFContactors   = 2
'DOFKnocker      = 2
'DOFChimes       = 2
'DOFBell         = 2
'DOFGear         = 2
'DOFShaker       = 2
'DOFFlippers     = 2
'DOFTargets      = 2
'DOFDropTargets  = 2
'
'Note that the value can be 0,1 or 2 (0 enables only digital sound, 1 only DOF and 2 both)
'
'If B2S.Server is setup but one doesn't want to use it, one should change the registry entry for ForceDisableB2S to 1
'
'
'Table script usage:
'
'This needs to be added on top of the script on both SS and EM tables:
'
'  On Error Resume Next
'  ExecuteGlobal GetTextFile("Controller.vbs")
'  If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the same folder as this table."
'  On Error Goto 0
'
'In addition the name of the rom (or the fake rom name for EM tables) is needed, because we need it for B2S (and loading VPM):
'
'  cGameName = "rom_name"
'
'For SS tables, the traditional LoadVPM method must be -removed- from the script
'as it is fully integrated into this script (leave the actual call in the script, of course),
'so search for something like this in the table script and -comment out or delete-:
'
'  Sub LoadVPM(VPMver, VBSfile, VBSver)
'    On Error Resume Next
'    If ScriptEngineMajorVersion <5 Then MsgBox "VB Script Engine 5.0 or higher required"
'    ExecuteGlobal GetTextFile(VBSfile)
'    If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description
'
'    Set Controller = CreateObject("B2S.Server")
'    'Set Controller = CreateObject("VPinMAME.Controller")
'
'    If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
'    If VPMver> "" Then If Controller.Version <VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
'    If VPinMAMEDriverVer <VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
'    On Error Goto 0
'  End Sub
'
'For SS tables with bad/outdated support by B2S Server (unsupported solenoids, lamps) one can call:
'
'  LoadVPMALT
'
'For EM tables, in the table_init, call:
'
'  LoadEM
'
'For PROC tables, in the table_init, call:
'
'  LoadPROC
'
'Finally, all calls to the B2S.Server Controller properties must be surrounded by a B2SOn check, so for example:
'
'  If B2SOn Then Controller.B2SSetGameOver 1
'
'Or "If ... End If" for multiple script lines that feature the B2S.Server Controller properties, for example:
'
'  If B2SOn Then
'    Controller.B2SSetTilt 0
'    Controller.B2SSetCredits Credits
'    Controller.B2SSetGameOver 1
'  End If
'
'That's all :)
'
'
'Optionally, if one wants to add the automatic ability to mute sounds and switch to DOF calls instead
'(based on the toy configuration that is set at the first run of a table), one can use three variants:
'
'For SS tables:
'
'  PlaySound SoundFX("sound", DOF_toy_category)
'
'If the specific DOF_toy_category (knocker, chimes, etc) is set to 1 in the Controller.txt,
'it will not play the sound but play "" instead.
'
'For EM tables, usually DOF calls are scripted and directly linked with a sound, so SoundFX and DOF can be combined to one method:
'
'  PlaySound SoundFXDOF("sound", DOFevent, State, DOF_toy_category)
'
'If the specific DOF_toy_category (knocker, chimes, etc) is set to 1 in the Controller.txt,
'it will not play the sound but just trigger the DOF call instead.
'
'For pure DOF calls without any sound (lights for example), the DOF method can be used:
'
'  DOF(DOFevent, State)
'

'Const directory = "HKEY_CURRENT_USER\SOFTWARE\Visual Pinball\Controller\"
Dim objShell
Dim PopupMessage
Dim B2SController
Dim Controller
'Const DOFContactors = 1
'Const DOFKnocker = 2
'Const DOFChimes = 3
'Const DOFBell = 4
'Const DOFGear = 5
'Const DOFShaker = 6
'Const DOFFlippers = 7
'Const DOFTargets = 8
'Const DOFDropTargets = 9
'Const DOFOff = 0
'Const DOFOn = 1
'Const DOFPulse = 2

'Dim DOFeffects(9)
Dim B2SOn
Dim B2SOnALT

Sub LoadEM
	LoadController("EM")
End Sub

Sub LoadPROC(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("PROC")
End Sub

Sub LoadVPM(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("VPM")
End Sub

'This is used for tables that need 2 controllers to be launched, one for VPM and the second one for B2S.Server
'Because B2S.Server can't handle certain solenoid or lamps, we use this workaround to communicate to B2S.Server and DOF
'By scripting the effects using DOFAlT and SoundFXDOFALT and B2SController
Sub LoadVPMALT(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("VPMALT")
End Sub

Sub LoadVBSFiles(VPMver, VBSfile, VBSver)
	On Error Resume Next
	If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
	ExecuteGlobal GetTextFile(VBSfile)
	If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description	
	InitializeOptions
End Sub

Sub LoadVPinMAME
	Set Controller = CreateObject("VPinMAME.Controller")
	If Err Then MsgBox "Can't load VPinMAME." & vbNewLine & Err.Description
	If VPMver > "" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
	If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
	On Error Goto 0
End Sub

'Try to load b2s.server and if not possible, load VPinMAME.Controller instead.
'The user can put a value of 1 for ForceDisableB2S, which will force to load VPinMAME or no controller for EM tables.
'Also defines the array of toy categories that will either play the sound or trigger the DOF effect.
Sub LoadController(TableType)
	Dim FileObj
	Dim DOFConfig
	Dim TextStr2
	Dim tempC
	Dim count
	Dim ISDOF
	Dim Answer
	
	B2SOn = False
	B2SOnALT = False
	tempC = 0
	on error resume next
	Set objShell = CreateObject("WScript.Shell")
	objShell.RegRead(directory & "ForceDisableB2S")
	If Err.number <> 0 Then
		PopupMessage = "This latest version of Controller.vbs stores its settings in the registry. To adjust the values, you must use VP 10.2 (or newer) and setup your configuration in the DOF section of the -Keys, Nudge and DOF- dialog of Visual Pinball."
		objShell.RegWrite directory & "ForceDisableB2S",0, "REG_DWORD"
		objShell.RegWrite directory & "DOFContactors",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFKnocker",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFChimes",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFBell",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFGear",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFShaker",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFFlippers",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFTargets",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFDropTargets",2, "REG_DWORD"
		MsgBox PopupMessage
	End If
	tempC = objShell.RegRead(directory & "ForceDisableB2S")
	DOFeffects(1)=objShell.RegRead(directory & "DOFContactors")
	DOFeffects(2)=objShell.RegRead(directory & "DOFKnocker")
	DOFeffects(3)=objShell.RegRead(directory & "DOFChimes")
	DOFeffects(4)=objShell.RegRead(directory & "DOFBell")
	DOFeffects(5)=objShell.RegRead(directory & "DOFGear")
	DOFeffects(6)=objShell.RegRead(directory & "DOFShaker")
	DOFeffects(7)=objShell.RegRead(directory & "DOFFlippers")
	DOFeffects(8)=objShell.RegRead(directory & "DOFTargets")
	DOFeffects(9)=objShell.RegRead(directory & "DOFDropTargets")
	Set objShell = nothing

	If TableType = "PROC" or TableType = "VPMALT" Then
		If TableType = "PROC" Then
			Set Controller = CreateObject("VPROC.Controller")
			If Err Then MsgBox "Can't load PROC"
		Else
			LoadVPinMAME
		End If		
		If tempC = 0 Then
			On Error Resume Next
			If Controller is Nothing Then
				Err.Clear
			Else
				Set B2SController = CreateObject("B2S.Server")
				If B2SController is Nothing Then
					Err.Clear
				Else
					B2SController.B2SName = B2ScGameName
					B2SController.Run()
					On Error Goto 0
					B2SOn = True
					B2SOnALT = True
				End If
			End If
		End If
	Else
		If tempC = 0 Then
			On Error Resume Next
			Set Controller = CreateObject("B2S.Server")
			If Controller is Nothing Then
				Err.Clear
				If TableType = "VPM" Then 
					LoadVPinMAME
				End If
			Else
				Controller.B2SName = cGameName
				If TableType = "EM" Then
					Controller.Run()
				End If
				On Error Goto 0
				B2SOn = True
			End If
		Else
			If TableType = "VPM" Then 
				LoadVPinMAME
			End If
		End If
		Set DOFConfig=Nothing
		Set FileObj=Nothing
	End If
End sub

'Additional DOF sound vs toy/effect helpers:

'Mostly used for SS tables, returns the sound to be played or no sound, 
'depending on the toy category that is set to play the sound or not.
'The trigger of the DOF Effect is set at the DOF method level
'because for SS tables we usually don't need to script the DOF calls.
'Just map the Solenoid, Switches and Lamps in the ini file directly.
Function SoundFX (Sound, Effect)
	If ((Effect = 0 And B2SOn) Or DOFeffects(Effect)=1) Then
		SoundFX = ""
	Else
		SoundFX = Sound
	End If
End Function

'Mostly used for EM tables, because in EM there is most often a direct link
'between a sound and DOF Trigger, DOFevent is the ID reference of the DOF Call
'that is used in the DOF ini file and State defines if it pulses, goes on or off.
'Example based on the constants that must be present in the table script:
'SoundFXDOF("flipperup",101,DOFOn,contactors)
Function SoundFXDOF (Sound, DOFevent, State, Effect)
	If DOFeffects(Effect)=1 Then
		SoundFXDOF = ""
		DOF DOFevent, State
	ElseIf DOFeffects(Effect)=2 Then
		SoundFXDOF = Sound
		DOF DOFevent, State
	Else
		SoundFXDOF = Sound
	End If
End Function

'Method used to communicate to B2SController instead of the usual Controller
Function SoundFXDOFALT (Sound, DOFevent, State, Effect)
	If DOFeffects(Effect)=1 Then
		SoundFXDOFALT = ""
		DOFALT DOFevent, State
	ElseIf DOFeffects(Effect)=2 Then
		SoundFXDOFALT = Sound
		DOFALT DOFevent, State
	Else
		SoundFXDOFALT = Sound
	End If
End Function

'Pure method that makes it easier to call just a DOF Event.
'Example DOF 123, DOFOn
'Where 123 refers to E123 in a line in the DOF ini.
Sub DOF(DOFevent, State)
	If B2SOn Then
		If State = 2 Then
			Controller.B2SSetData DOFevent, 1:Controller.B2SSetData DOFevent, 0
		Else
			Controller.B2SSetData DOFevent, State
		End If
	End If
End Sub

'If PROC or B2SController is used, we need to pass B2S events to the B2SController instead of the usual Controller
'Use this method to pass information to DOF instead of the Sub DOF
Sub DOFALT(DOFevent, State)
	If B2SOnALT Then
		If State = 2 Then
			B2SController.B2SSetData DOFevent, 1:B2SController.B2SSetData DOFevent, 0
		Else
			B2SController.B2SSetData DOFevent, State
		End If
	End If
End Sub


'XXXXXX

'recordspin.enabled = 1
'
'Sub recordspin_timer
'	if TurntableTimer.enabled then
'		recordplate.RotZ = recordplate.RotZ + 1
'	end if
'End Sub
'

dim MagnetW
dim MagnetW2
dim spinner

	 Set MagnetW = New cvpmMagnet
		With MagnetW
			.InitMagnet Magnetwall, 60
			.GrabCenter = True
			.MagnetOn = 0
			.CreateEvents "MagnetW"
		End With

	 Set MagnetW2 = New cvpmMagnet
		With MagnetW2
			.InitMagnet Magnetwall1, 60
			.GrabCenter = True
			.MagnetOn = 0
			.CreateEvents "MagnetW2"
		End With

	 Set spinner = New cvpmTurntable
		With spinner
			.InitTurntable TurnTable, 60
			.SpinDown = 10
			.CreateEvents "spinner"
		End With




Sub bumperflashing
	flashtimer.enabled = 1
end Sub

	dim flashpos:flashpos = 0
	Sub flashtimer_timer
		flashpos = flashpos + 1
		select case flashpos
			case 1
				centershine.state = 1
				centershine2.state = 0
				centershine3.opacity = 1000
			case 2
				centershine.state = 0
				centershine2.state = 1
				centershine3.opacity = 0
			case 3
				centershine.state = 1
				centershine2.state = 0
				centershine3.opacity = 1000
			case 4
				centershine.state = 0
				centershine2.state = 1
				centershine3.opacity = 0
			case 5
				centershine.state = 1
				centershine2.state = 0
				centershine3.opacity = 1000
			case 6
				centershine.state = 0
				centershine2.state = 1
				centershine3.opacity = 0
			case 7
				centershine.state = 1
				centershine2.state = 0
				centershine3.opacity = 1000
			case 8
				centershine.state = 0
				centershine2.state = 1
				centershine3.opacity = 0
			case 9
				centershine.state = 1
				centershine2.state = 0
				centershine3.opacity = 1000
			case 10
				centershine.state = 0
				centershine2.state = 1
				centershine3.opacity = 0
			case 11
				centershine.state = 0
				centershine2.state = 0
				centershine3.opacity = 0
				flashtimer.enabled = 0
				flashpos = 0
		end Select
	End Sub



Sub leftflashing
	lefttimer.enabled = 1
end Sub

	dim leftpos:leftpos = 0
	Sub lefttimer_timer
		leftpos = leftpos + 1
		select case leftpos
			case 1
				leftshine.state = 1
				leftshine2.state = 0
			case 2
				leftshine.state = 0
				leftshine2.state = 1
			case 4
				leftshine.state = 1
				leftshine2.state = 0
			case 5
				leftshine.state = 0
				leftshine2.state = 1
			case 6
				leftshine.state = 1
				leftshine2.state = 0
			case 7
				leftshine.state = 0
				leftshine2.state = 1
			case 8
				leftshine.state = 1
				leftshine2.state = 0
			case 9
				leftshine.state = 0
				leftshine2.state = 1
			case 10
				leftshine.state = 1
				leftshine2.state = 0
			case 11
				leftshine.state = 0
				leftshine2.state = 1
			case 12
				leftshine.state = 0
				leftshine2.state = 0
				lefttimer.enabled = 0
				leftpos = 0
		end Select
	End Sub





Sub rightflashing
	righttimer.enabled = 1
end Sub

	dim rightpos:rightpos = 0
	Sub righttimer_timer
		rightpos = rightpos + 1
		select case rightpos
			case 1
				rightshine.state = 1
				rightshine2.state = 0
			case 2
				rightshine.state = 0
				rightshine2.state = 1
			case 4
				rightshine.state = 1
				rightshine2.state = 0
			case 5
				rightshine.state = 0
				rightshine2.state = 1
			case 6
				rightshine.state = 1
				rightshine2.state = 0
			case 7
				rightshine.state = 0
				rightshine2.state = 1
			case 8
				rightshine.state = 1
				rightshine2.state = 0
			case 9
				rightshine.state = 0
				rightshine2.state = 1
			case 10
				rightshine.state = 1
				rightshine2.state = 0
			case 11
				rightshine.state = 0
				rightshine2.state = 1
			case 12
				rightshine.state = 0
				rightshine2.state = 0
				righttimer.enabled = 0
				rightpos = 0
		end Select
	End Sub


Sub right2flashing
	righttimer1.enabled = 1
end Sub

	dim right2pos:right2pos = 0
	Sub righttimer1_timer
		right2pos = right2pos + 1
		select case right2pos
			case 1
				rightshine4.state = 1
				rightshine5.state = 0
			case 2
				rightshine4.state = 0
				rightshine5.state = 1
			case 4
				rightshine4.state = 1
				rightshine5.state = 0
			case 5
				rightshine4.state = 0
				rightshine5.state = 1
			case 6
				rightshine4.state = 1
				rightshine5.state = 0
			case 7
				rightshine4.state = 0
				rightshine5.state = 1
			case 8
				rightshine4.state = 1
				rightshine5.state = 0
			case 9
				rightshine4.state = 0
				rightshine5.state = 1
			case 10
				rightshine4.state = 1
				rightshine5.state = 0
			case 11
				rightshine4.state = 0
				rightshine5.state = 1
			case 12
				rightshine4.state = 0
				rightshine5.state = 0
				righttimer1.enabled = 0
				right2pos = 0
		end Select
	End Sub

	Sub gate2_hit
		FlashLevel2 = 1 : Flasherflash2_Timer
	End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  FRAMEWORK Options
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	Const typefont = "HUN-din 1451"
	Const numberfont = "HUN-din 1451"
	Const zoomfont = "HUN-din 1451"
	Const zoombgfont = "HUN-din 1451" ' needs to be an outline of the zoomfont
	Const infofont = "FORTE"
	Const TableName = "seawitfp"



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Pinup Active Backglass
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'**************************
'   PinUp Player Config
'   Change HasPuP = True if using PinUp Player Videos
'**************************

Dim HasPup:HasPuP = True

Dim PuPlayer

Const pTopper=0
Const pDMD=1
Const pBackglass=2
Const pPlayfield=3
'Const pScenes=5
Const pMusic=4
Const pAudio=9
Const pCallouts=8
Const pVids=2
Const pModes=2

Sub chilloutthemusic
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":10 }"
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":10 }"
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 9, ""FN"":11, ""VL"":10 }"
	vpmtimer.addtimer 2200, "turnitbackup '"
End Sub

Sub turnitbackup
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":99 }"
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":99 }"
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 9, ""FN"":11, ""VL"":99 }"
End Sub

Sub PupInit
if HasPuP Then
	on error resume next
	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
	on error goto 0
	if not IsObject(PuPlayer) then 
		HasPuP = False
	end If
end If

if HasPuP Then
	PuPlayer.Init pBackglass,cGameName
	PuPlayer.Init pVids,cGameName
	PuPlayer.Init pMusic,cGameName
	PuPlayer.Init pAudio,cGameName
	PuPlayer.Init pCallouts,cGameName
	'PuPlayer.Init pScenes,cGameName
	PuPlayer.Init pModes,cGameName

	If toppervideo = 1 Then
		PuPlayer.Init pTopper,cGameName
	End If

	PuPlayer.SetScreenex pBackglass,0,0,0,0,1       'Set PuPlayer DMD TO Always ON    <screen number> , xpos, ypos, width, height, POPUP
	PuPlayer.SetScreenex pAudio,0,0,0,0,2
	PuPlayer.SetScreenex pVids,0,0,0,0,0
'	PuPlayer.SendMSG "{ 'mt':301, 'SN': 2, 'FN':15, 'CP':'99,15,15,70,70' }"  'custompos
	'PuPlayer.SetScreenex pScenes,0,0,0,0,0
	'PuPlayer.SendMSG "{ 'mt':301, 'SN': 5, 'FN':15, 'CP':'99,15,15,70,70' }"  'custompos
	PuPlayer.hide pAudio
	PuPlayer.SetScreenex pMusic,0,0,0,0,2
	PuPlayer.hide pMusic
	PuPlayer.SetScreenex pCallouts,0,0,0,0,2
	PuPlayer.hide pCallouts

	PuPlayer.SetScreenex pModes,0,0,0,0,0


	PuPlayer.playlistadd pMusic,"audioattract", 1 , 0
	PuPlayer.playlistadd pAudio,"audioevents", 1 , 0
	PuPlayer.playlistadd pCallouts,"audiocallouts", 1 , 0
	PuPlayer.playlistadd pVids,"backglass", 1 , 0
	PuPlayer.playlistadd pBackglass,"PuPOverlays", 1 , 0
	PuPlayer.playlistadd pVids,"videoattract", 1 , 0
	PuPlayer.playlistadd pModes,"modes", 1 , 0
	'PuPlayer.playlistadd pScenes,"scenes", 1 , 0


	PuPlayer.LabelInit pBackglass

	If toppervideo = 1 Then
		PuPlayer.playlistadd pTopper,"topper", 1 , 0
	End If

	'Set Background video on DMD
	PuPlayer.playlistplayex pVids,"videoattract","",videovolume,1
	PuPlayer.SetBackground pVids,1	
	if noscores = 0 then
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","overlay.png",0,1
	else
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","overlaynoscore.png",0,1
	end if				
	End if

	If toppervideo = 1 Then
		PuPlayer.playlistplayex pTopper,"topper","",0,1
		PuPlayer.SetBackground pTopper,1	
	End If


	'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS
	'syntax - PuPlayer.LabelNew <screen# or pDMD>,<Labelname>,<fontName>,<size%>,<colour>,<rotation>,<xAlign>,<yAlign>,<xpos>,<ypos>,<PageNum>,<visible>

	'Page 1 (default score display)
	If noscores = 0 then
'		PuPlayer.LabelNew pBackglass,"Play1score",numberfont,	5,0  ,0,0,1,26,85,1,0	'7
'		PuPlayer.LabelNew pBackglass,"Play2score",numberfont,	5,0  ,0,0,1,55,85,1,0
'		PuPlayer.LabelNew pBackglass,"Play3score",numberfont,	5,0  ,0,0,1,26,95,1,0
'		PuPlayer.LabelNew pBackglass,"Play4score",numberfont,	5,0  ,0,0,1,55,95,1,0

' Aligned Points with delimiters
		PuPlayer.LabelNew pBackglass,"Play1score",numberfont,	5,0  ,0,2,1,44,85,1,0	'7
		PuPlayer.LabelNew pBackglass,"Play2score",numberfont,	5,0  ,0,2,1,73,85,1,0
		PuPlayer.LabelNew pBackglass,"Play3score",numberfont,	5,0  ,0,2,1,44,95,1,0
		PuPlayer.LabelNew pBackglass,"Play4score",numberfont,	5,0  ,0,2,1,73,95,1,0

'		PuPlayer.LabelNew pBackglass,"curscore",numberfont,		5,0	 ,0,1,1,50,87,1,1
		PuPlayer.LabelNew pBackglass,"Ball",typefont,			4,0  ,0,1,1,50,91,1,1	'5
	end if
	PuPlayer.LabelNew pBackglass,"titlebg",zoombgfont,		9,0  	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"title",zoomfont,			9,0 	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"titlebg2",zoombgfont,		6,0  	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"title2",zoomfont,			6,0 	,0,1,1,50,50,1,1

' Additional Info for Modes and Mystery Target
	PuPlayer.LabelNew pBackglass,"ModeInfobg",infofont,			9, 4777215	,0,1,1,50,30,1,1
	PuPlayer.LabelNew pBackglass,"ModeInfo2bg",infofont,		7, 4777215	,0,1,1,50,40,1,1
	PuPlayer.LabelNew pBackglass,"ModeLevelbg",infofont,		7, 4777215	,0,0,1,24,73,1,1
	PuPlayer.LabelNew pBackglass,"ModeTimerbg",infofont,		6, 4777215	,0,2,1,75,73,1,1
	PuPlayer.LabelNew pBackglass,"GameOverbg",infofont,			13, 255	,0,1,1,50,48,1,1
	PuPlayer.LabelNew pBackglass,"Skillshot1bg",infofont,		15, 4778515	,0,1,1,50,30,1,1
	PuPlayer.LabelNew pBackglass,"Skillshot2bg",infofont,		15, 77749231	,0,1,1,50,30,1,1
	PuPlayer.LabelNew pBackglass,"Skillshot3bg",infofont,		15, 13777215	,0,1,1,50,30,1,1
	PuPlayer.LabelNew pBackglass,"SkillshotInfo1bg",infofont,		10,4778515 	,0,1,1,50,68,1,1
	PuPlayer.LabelNew pBackglass,"SkillshotInfo2bg",infofont,		10,77749231 	,0,1,1,50,68,1,1
	PuPlayer.LabelNew pBackglass,"SkillshotInfo3bg",infofont,		10,13777215 	,0,1,1,50,68,1,1
	PuPlayer.LabelNew pBackglass,"MysteryInfobg",infofont,		6, 12777215 ,0,1,1,50,57,1,1
	PuPlayer.LabelNew pBackglass,"MysteryInfo2bg",infofont,		10,12777215 	,0,1,1,50,65,1,1
	PuPlayer.LabelNew pBackglass,"BallSavedbg",infofont,		10,15777215 	,0,1,1,50,48,1,1

	PuPlayer.LabelNew pBackglass,"Jackpotbg",infofont,		8,65535 	,0,1,1,50,56,1,1
	PuPlayer.LabelNew pBackglass,"Jackpot2bg",infofont,		10,65535 	,0,1,1,50,65,1,1
End Sub
'ccc
'Colours
' 16777215 - white
' 1577725 - red
' 77749231 - pink
' 15777215 - light blue
' 13777215 - blue
' 4777215 - yellow
' 4778515 - green
' 12777215 - aged white

'255 blue   - 16711680
'255 red    - 255
'255 green  - 65280
'255 yellow - 65535


Sub resetbackglass
	PuPlayer.LabelShowPage pBackglass,1,0,""
	PuPlayer.playlistplayex pBackglass,"scene","layout.png",0,1
	PuPlayer.SetBackground pBackglass,1
	pUpdateScores
End Sub

Dim titlepos
titlepos = 0
titletimer.enabled = 0
dim title
title = ""
dim subtitle
subtitle = ""

Sub titletimer_timer
	titlepos = titlepos + 1
	Select Case titlepos
		Case 1
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':2565927, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':2565927, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 2
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':5066061, 'size': 0.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 0.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':5066061, 'size': 0.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 3
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':7960953, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':7960953, 'size': 0.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 4
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':9671571, 'size': 1.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 1.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':9671571, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 5
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':11842740, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':11842740, 'size': 1.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 1.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 6
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':13224393, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':13224393, 'size': 2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 7
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':14671839, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':14671839, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 8
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':15790320, 'size': 4.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 4.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':15790320, 'size': 2.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 9
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16316664, 'size': 4.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 4.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16316664, 'size': 3.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 3.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 10
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16777215, 'size': 5.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 5.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16777215, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 11
			PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		Case 150
			PuPlayer.LabelSet pBackglass,"title","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg","",1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2","",1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2","",1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			titlepos = 0
			titletimer.enabled = 0
	End Select
End Sub


Sub pNote(msgText,msg2text)
	title = msgText
	subtitle = msg2text
	If titlepos = 0 Then
		titletimer.enabled = 1
	Else
		titlepos = 0
		titletimer.enabled = 1
		PuPlayer.LabelSet pBackglass,"title","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
		PuPlayer.LabelSet pBackglass,"titlebg","",1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
		PuPlayer.LabelSet pBackglass,"title2","",1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		PuPlayer.LabelSet pBackglass,"titlebg2","",1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
	End If
End Sub

Sub currentplayerbackglass

End Sub

Dim firstball:firstball = 0

Sub pupballstart_hit
	If KickSavedBall Then
		Plunger.AutoPlunger = true
		Plunger.Pullback
		Plunger.Fire
		PlaySound"plunger"
		Plunger.AutoPlunger = false
		KickSavedBall = False
	end If
End Sub

'separated to avoid video restart on returning ball in plunger lane
Sub pupballstart1_hit
	if (not Mode4Timer.enabled) and ((not l19641.state) or (not l19649.state) or (not l19646.state) or (l19644.state)) then
		spinflash.enabled = 0
		spinflashright.visible = 0
		spinflashleft.visible = 0
		Spinnerbonus = False
	end if
	firstball = firstball + 1
	select case firstball
		case 1
		PuPlayer.playlistplayex pCallouts,"audiocallouts","ladies and gentleman the beatles.mp3",calloutvolume,1
	end select 
	If not KickSavedBall Then
		if dsv = 0 Then
			PuPlayer.playlistplayex pVids,"backglass","",videovolume,1
			PuPlayer.SetLoop 9,1
			dsv = 1
		end if
	end If
End Sub

Sub predrain_hit
	If (BSoffTimer.enabled) or (MysteryBSoffTimer.enabled) or (BSGraceTimer.enabled) Then
		BallsSaved = BallsSaved + 1
		if BallsSaved > 1 Then	
			bsoff
		end if
		predrain.DestroyBall
		playsound "drain"
		vpmtimer.addtimer 1500, "Addsavedball '"
	Else
		if bip > 1 Then
			if BeatlemaniaMBTimer.enabled Then
				bip = bip - 1
				playsound"drain"
				predrain.DestroyBall
				if bip = 1 Then
					EndMode
				end If
			Else			
				if bip = 2 Then
					if haspup Then
						if not ModeActiveTimer.enabled then
							PuPlayer.LabelSet pBackglass,"ModeInfobg","",1,""
							PuPlayer.playlistplayex pVids,"backglass","",videovolume,1		
						end If	
					end If
					if not Modecompleted(PlayerNo,1) then
						Modecompleted(PlayerNo,1) = True
						ResetDropTargets
						Mode1Light.state = Lightstateon
						if Modecompleted(PlayerNo,1) and Modecompleted(PlayerNo,2) and Modecompleted(PlayerNo,3) and Modecompleted(PlayerNo,4) and Modecompleted(PlayerNo,5) and (not Modecompleted(PlayerNo,0)) then
							ModeReady = 6
							Mode1Light.state = Lightstateblinking
							Mode2Light.state = Lightstateblinking
							Mode3Light.state = Lightstateblinking
							Mode4Light.state = Lightstateblinking
							Mode5Light.state = Lightstateblinking
						end If
					end if
				end if
				bip = bip - 1
				playsound"drain"
				predrain.DestroyBall
			end if
		Else
			bspos = 0
			spinflash.enabled = 0
			spinflashright.visible = 0
			spinflashleft.visible = 0
			Spinnerbonus = False
			addtllightsoff

			'sorry, no saved locked ball for consistent multiplayer games
			if not superjlock.isdropped Then
				superjlock.isdropped = True
				if ballinlock = 1 Then
					predrain.DestroyBall
					ballinlock = 0
					sw38.isdropped = false:sw39.isdropped = false:sw40.isdropped = false
					Playsound SoundFX("DTReset",DOFContactors)
					fabf.state = 0:faba.state = 0:fabb.state = 0
				end If
			end If
		End If
	end If
End Sub

Dim KickSavedBall:KickSavedBall = False
Dim NoExploit:NoExploit = False

Sub AddSavedBall
	KickSavedBall = True
	NoExploit = True
	NoExploitTimer.enabled = True	
	ballrelease.createball:BallRelease.Kick 90, 4
	if HasPup Then
		PuPlayer.LabelSet pBackglass,"BallSavedbg","Ball Saved!",1,""
		vpmtimer.addtimer 2000, "PuPlayer.LabelSet pBackglass,""BallSavedbg"","""",1,"""" '"
	end if
End Sub

Sub addtllightsoff
	dim a,i
	For each a in addlights:a.State = 0: Next
	For i = 0 to 10:DT_State(i) = 0: Next
End Sub

Sub bsactive_hit
	If (bspos = 1) and (not BSoffTimer.enabled) and (not MysteryBSoffTimer.enabled) and (not BSGraceTimer.enabled) Then
		BSoffTimer.enabled = True
		'dsv = 0
	End If
	if NoExploit Then
		NoExploit = False
		NoExploitTimer.enabled = False
		NoExploitTimer.enabled = True
	end If
End Sub

Dim bspos:bspos = 0

Sub BSoffTimer_Timer
	BSoffTimer.enabled = False
	ballsave.state = 0
	bspos = 0
	BSGraceTimer.enabled = True
End Sub

Sub MysteryBSoffTimer_Timer
	MysteryBSoffTimer.enabled = False
	ballsave.state = 0
	bspos = 0
	BSGraceTimer.enabled = True	
End Sub

Sub BSGraceTimer_Timer
	bsoff
End Sub

Sub bsoff
	BSoffTimer.enabled = False
	MysteryBSoffTimer.enabled = False
	BSGraceTimer.enabled = False	
	ballsave.state = 0
	bspos = 0
end sub

sub checkfab
	if (fabf.state=1) and (faba.state=1) and (fabb.state=1) Then
		vpmtimer.addtimer 1000, "dropem '"
		sw38.isdropped = True:sw39.isdropped = true:sw40.isdropped = True
		superj.state = 2
'		if ballinlock = 1 Then
'			superjlock.isdropped = true
'			bip = 2
'			ballinlock = 0
'			Mode1Light.state = Lightstateblinking
'			if HasPup Then
'				if not ModeActiveTimer.enabled then
'					PuPlayer.LabelSet pBackglass,"ModeInfobg","2-Ball Multiball",1,""
'					PuPlayer.playlistplayex pBackglass,"modes","1_All My Loving.mp4",videovolume,1
'				end If
'			end If
'		End If
	end If
end Sub

sub dropem
	sw38.isdropped = True:sw39.isdropped = true:sw40.isdropped = True
end Sub

dim ballinlock:ballinlock = 0
dim bip:bip = 1
dim dsv:dsv = 0
MagnetW.MagnetOn = False

sub superjt_hit
	if superj.state = lightstateblinking Then
		MagnetW.MagnetOn = True
		vpmtimer.addtimer 500, "magrelease '"
		PlaySound "beep beep"
			if ballinlock = 1 Then
				superjlock.isdropped = true
				bip = 2
				ballinlock = 0
				Mode1Light.state = Lightstateblinking
				if HasPup Then
					if not ModeActiveTimer.enabled then
						PuPlayer.LabelSet pBackglass,"ModeInfobg","2-Ball Multiball",1,""
						PuPlayer.playlistplayex pBackglass,"modes","1_All My Loving.mp4",videovolume,1
					end If
				end If
			End If
		flashdrum
		sw38.isdropped = false:sw39.isdropped = false:sw40.isdropped = false
		Playsound SoundFX("DTReset",DOFContactors)
		fabf.state = 0:faba.state = 0:fabb.state = 0
		playsound SoundFX("Knocker",DOFKnocker)
		AddPoints Jackpotvalue(PlayerNo)
		superj.state = lightstateoff
		if HasPup Then
'			if not ModeActiveTimer.enabled then
				PuPlayer.LabelSet pBackglass,"Jackpotbg","!!! JACKPOT !!!",1,""
				PuPlayer.LabelSet pBackglass,"Jackpot2bg",FormatPointText(cstr(Jackpotvalue(PlayerNo))),1,""
				vpmtimer.addtimer 2500, "PuPlayer.LabelSet pBackglass,""Jackpotbg"","""",1,"""" '"
				vpmtimer.addtimer 2500, "PuPlayer.LabelSet pBackglass,""Jackpot2bg"","""",1,"""" '"
'			end If
		end If

		if bip = 1 then
			superjlock.isdropped = false
			Playsound SoundFX("DTReset",DOFContactors)
		end if
	Else
		'Sneak In
		AddPoints 5000
		if HasPup Then
			PuPlayer.LabelSet pBackglass,"Jackpotbg","Sneak In",1,""
			PuPlayer.LabelSet pBackglass,"Jackpot2bg","5.000",1,""
			vpmtimer.addtimer 2500, "PuPlayer.LabelSet pBackglass,""Jackpotbg"","""",1,"""" '"
			vpmtimer.addtimer 2500, "PuPlayer.LabelSet pBackglass,""Jackpot2bg"","""",1,"""" '"
		end If
	End If
end sub

sub magrelease
	MagnetW.MagnetOn = False
end sub

sub releaser_hit
	If ballinlock = 1 Then
			superjlock.isdropped = true
			bip = 2
			ballinlock = 0
			sw38.isdropped = false:sw39.isdropped = false:sw40.isdropped = false
			Playsound SoundFX("DTReset",DOFContactors)
			fabf.state = 0:faba.state = 0:fabb.state = 0
			Mode1Light.state = Lightstateblinking
			if HasPup Then
				if not ModeActiveTimer.enabled then
					PuPlayer.LabelSet pBackglass,"ModeInfobg","2-Ball Multiball",1,""
					PuPlayer.playlistplayex pBackglass,"modes","1_All My Loving.mp4",videovolume,1
				end If
			end If
	else

	End If
end Sub

Sub superjball_hit
	If superjlock.isdropped = false Then
'		dsv = 1
		ballinlock = 1
		ballrelease.createball:BallRelease.Kick 90, 4
		KickSavedBall = True
		NoExploit = True
		NoExploitTimer.enabled = True	
	End If
End Sub

Sub flashdrum
	drumtime.enabled = 1
end Sub

dim drumpos:drumpos = 0
Sub drumtime_timer
	drumpos = drumpos + 1
	select case drumpos
		case 1
			drum.state = 1
		case 2
			drum.state = 0
		case 3
			drum.state = 1
		case 4
			drum.state = 0
		case 5
			drum.state = 1
		case 6
			drum.state = 0
		case 7
			drum.state = 1
		case 8
			drum.state = 0
			drumtime.enabled = 0
			drumpos = 0
	end Select
End Sub

Dim scores(32)
dim pp
For pp = 0 to 32
scores(pp) = " "
next

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,statdupe,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2) : statdupe = chgLED(ii, 2)
				if (num < 32) then
					If DesktopMode = True Then
					For Each obj In Digits(num)
						If chg And 1 Then obj.State = stat And 1 
						chg = chg\2 : stat = stat\2
					Next
					End If
					debug.print "stat = " & (statdupe and 127)
					Select Case (statdupe and 127)
						Case 0 : scores(num) = " ":debug.print " "
						Case 63 : scores(num) = "0":debug.print "0"
						Case 6 : scores(num) = "1":debug.print "1"
						Case 91 : scores(num) = "2":debug.print "2"
						Case 79 : scores(num) = "3":debug.print "3"
						Case 102 : scores(num) = "4":debug.print "4"
						Case 109 : scores(num) = "5":debug.print "5"
						Case 125 : scores(num) = "6":debug.print "6"
						Case 7 : scores(num) = "7":debug.print "7"
						Case 127 : scores(num) = "8":debug.print "8"
						Case 111 : scores(num) = "9":debug.print "9"
					End Select
					debug.print "scores num = " & scores(num) & " num:" & num
					pUpdateScores
				end if
			next
	end if
End Sub

Dim gameon:gameon = 0
Sub pUpdateScores
	if noscores = 0 then
		PuPlayer.LabelSet pBackglass,"play1score",FormatPointText(ltrim(cstr(cstr(scores(0) & scores(1) & scores(2) & scores(3) & scores(4) & scores(5) & scores(6))))),1,""
		PuPlayer.LabelSet pBackglass,"play2score",FormatPointText(ltrim(cstr(scores(7) & scores(8) & scores(9) & scores(10) & scores(11) & scores(12) & scores(13)))),1,""
		PuPlayer.LabelSet pBackglass,"play3score",FormatPointText(ltrim(cstr(scores(14) & scores(15) & scores(16) & scores(17) & scores(18) & scores(19) & scores(20)))),1,""
		PuPlayer.LabelSet pBackglass,"play4score",FormatPointText(ltrim(cstr(scores(21) & scores(22) & scores(23) & scores(24) & scores(25) & scores(26) & scores(27)))),1,""
		PuPlayer.LabelSet pBackglass,"Ball",scores(30) & scores(31),1,""
	end if
	If scores(30) = " " Then
			gameon = 1
		Else
			gameon = 0
	End If
end Sub

Dim PointsText,TempText, i
Function FormatPointText(ScorePar)
	PointsText = ""
	TempText = ScorePar
	for i = 1 to 10
		if len(temptext) > 3 then
			PointsText = "." & right(TempText,3) & Pointstext
			TempText = left(Temptext,len(TempText)-3)
		else
			if len(Temptext) > 0 then
				PointsText = TempText & Pointstext
				TempText = ""
			end if	
		end if
	next
	FormatPointText = PointsText
End Function


dim ingame:ingame = 0
sub gamechange_timer
	if ingame = gameon Then
	Else
		if gameon = 0 Then
			ingame = 0
			'PuPlayer.playlistplayex pScenes,"scenes","gameover.mp4",0,1
			vpmtimer.addtimer 2000, "startattract '"
		Else
			ingame = 1
		end If
	end if

end Sub

sub startattract
	PuPlayer.playlistplayex pBackglass,"videoattract","",99,1
End Sub


sub flash1_hit 
	strip1.visible = 1
	f4.visible = 1
	vpmtimer.addtimer 200, "f1off '"
End Sub
sub f1off
	strip1.visible = 0
	f4.visible = 0
end Sub

sub flash2_hit 
	strip3.visible = 1
	f4.visible = 1
FlashLevel7 = 1 : Flasherflash7_Timer
	vpmtimer.addtimer 200, "f2off '"
End Sub
sub f2off
	strip3.visible = 0
end Sub

sub flash3_hit 
	strip5.visible = 1
	f5.visible = 1
	vpmtimer.addtimer 200, "f3off '"
FlashLevel3 = 1 : Flasherflash3_Timer
FlashLevel7 = 1 : Flasherflash7_Timer
End Sub
sub f3off
	Strip5.visible = 0
	f4.visible = 0
	f5.visible = 0
end Sub

sub flash4_hit 
	Strip7.visible = 1
	f5.visible = 1
FlashLevel3 = 1 : Flasherflash3_Timer
	vpmtimer.addtimer 200, "f4off '"
End Sub
sub f4off
	Strip7.visible = 0
end Sub

sub flash5_hit 
	strip9.visible = 1
	f5.visible = 1
	vpmtimer.addtimer 200, "f5off '"
End Sub
sub f5off
	strip9.visible = 0
	f5.visible = 0
end Sub

Sub quotes_timer
	if (not ModeActiveTimer.enabled) and (bip < 2) and (not LampState(45)) then
		PuPlayer.playlistplayex pCallouts,"audiocallouts","",calloutvolume,1		
	end If	
end sub


Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6, FlashLevel7
Flasherlight6.IntensityScale = 0
Flasherlight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
Flasherlight7.IntensityScale = 0

'*** left white flasher ***
Sub Flasherflash5_Timer()
	dim flashx3, matdim
	If not Flasherflash5.TimerEnabled Then 
		Flasherflash5.TimerEnabled = True
		Flasherflash5.visible = 1
		Flasherlit5.visible = 1
	End If
	flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
	Flasherflash5.opacity = 500 * flashx3
	Flasherlit5.BlendDisableLighting = 10 * flashx3
	Flasherbase5.BlendDisableLighting =  flashx3
	Flasherlight6.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel5)
	'Flasherlit5.material = "domelit" & matdim
	FlashLevel5 = FlashLevel5 * 0.9 - 0.01
	If FlashLevel5 < 0.15 Then
		Flasherlit5.visible = 0
	Else
		Flasherlit5.visible = 1
	end If
	If FlashLevel5 < 0 Then
		Flasherflash5.TimerEnabled = False
		Flasherflash5.visible = 0
	End If
End Sub

'*** left white flasher ***
Sub Flasherflash1_Timer()
	dim flashx3, matdim
	If not Flasherflash1.TimerEnabled Then 
		Flasherflash1.TimerEnabled = True
		Flasherflash1.visible = 1
		Flasherlit1.visible = 1
	End If
	flashx3 = FlashLevel1 * FlashLevel1 * FlashLevel1
	Flasherflash1.opacity = 500 * flashx3
	Flasherlit1.BlendDisableLighting = 10 * flashx3
	Flasherbase1.BlendDisableLighting =  flashx3
	Flasherlight1.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel1)
	'Flasherlit1.material = "domelit" & matdim
	FlashLevel1 = FlashLevel1 * 0.9 - 0.01
	If FlashLevel1 < 0.15 Then
		Flasherlit1.visible = 0
	Else
		Flasherlit1.visible = 1
	end If
	If FlashLevel1 < 0 Then
		Flasherflash1.TimerEnabled = False
		Flasherflash1.visible = 0
	End If
End Sub

'*** left white flasher ***
Sub Flasherflash2_Timer()
	dim flashx3, matdim
	If not Flasherflash2.TimerEnabled Then 
		Flasherflash2.TimerEnabled = True
		Flasherflash2.visible = 1
		Flasherlit2.visible = 1
	End If
	flashx3 = FlashLevel2 * FlashLevel2 * FlashLevel2
	Flasherflash2.opacity = 500 * flashx3
	Flasherlit2.BlendDisableLighting = 10 * flashx3
	Flasherbase2.BlendDisableLighting =  flashx3
	Flasherlight2.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel2)
	'Flasherlit2.material = "domelit" & matdim
	FlashLevel2 = FlashLevel2 * 0.9 - 0.01
	If FlashLevel2 < 0.15 Then
		Flasherlit2.visible = 0
	Else
		Flasherlit2.visible = 1
	end If
	If FlashLevel2 < 0 Then
		Flasherflash2.TimerEnabled = False
		Flasherflash2.visible = 0
	End If
End Sub


'*** left white flasher ***
Sub Flasherflash3_Timer()
	dim flashx3, matdim
	If not Flasherflash3.TimerEnabled Then 
		Flasherflash3.TimerEnabled = True
		Flasherflash3.visible = 1
		Flasherlit3.visible = 1
	End If
	flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
	Flasherflash3.opacity = 500 * flashx3
	Flasherlit3.BlendDisableLighting = 10 * flashx3
	Flasherbase3.BlendDisableLighting =  flashx3
	Flasherlight3.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	'Flasherlit2.material = "domelit" & matdim
	FlashLevel3 = FlashLevel3 * 0.9 - 0.01
	If FlashLevel3 < 0.15 Then
		Flasherlit3.visible = 0
	Else
		Flasherlit3.visible = 1
	end If
	If FlashLevel3 < 0 Then
		Flasherflash3.TimerEnabled = False
		Flasherflash3.visible = 0
	End If
End Sub

'*** left white flasher ***
Sub Flasherflash7_Timer()
	dim flashx3, matdim
	If not Flasherflash7.TimerEnabled Then 
		Flasherflash7.TimerEnabled = True
		Flasherflash7.visible = 1
		Flasherlit7.visible = 1
	End If
	flashx3 = FlashLevel7 * FlashLevel7 * FlashLevel7
	Flasherflash7.opacity = 500 * flashx3
	Flasherlit7.BlendDisableLighting = 10 * flashx3
	Flasherbase7.BlendDisableLighting =  flashx3
	Flasherlight7.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel7)
	'Flasherlit2.material = "domelit" & matdim
	FlashLevel7 = FlashLevel7 * 0.9 - 0.01
	If FlashLevel7 < 0.15 Then
		Flasherlit7.visible = 0
	Else
		Flasherlit7.visible = 1
	end If
	If FlashLevel7 < 0 Then
		Flasherflash7.TimerEnabled = False
		Flasherflash7.visible = 0
	End If
End Sub

Dim flashseq
flashseq = 0

Sub flashflash_Timer()
	flashseq = flashseq + 1
	Select Case flashseq
		Case 1
			FlashLevel5 = 1 : Flasherflash5_Timer
		Case 2
			FlashLevel1 = 1 : Flasherflash1_Timer
		Case 3
			FlashLevel2 = 1 : Flasherflash2_Timer
		Case 4
			FlashLevel4 = 1 : Flasherflash4_Timer
		Case 5
			FlashLevel6 = 1 : Flasherflash6_Timer
		Case 6
			FlashLevel3 = 1 : Flasherflash3_Timer
		Case 7
			FlashLevel5 = 1 : Flasherflash5_Timer
		Case 8
			FlashLevel1 = 1 : Flasherflash1_Timer
		Case 9
			FlashLevel2 = 1 : Flasherflash2_Timer
		Case 10
			FlashLevel4 = 1 : Flasherflash4_Timer
		Case 11
			FlashLevel6 = 1 : Flasherflash6_Timer
		Case 12
			FlashLevel3 = 1 : Flasherflash3_Timer
		Case 13
			FlashLevel5 = 1 : Flasherflash5_Timer
		Case 14
			FlashLevel1 = 1 : Flasherflash1_Timer
		Case 15
			FlashLevel2 = 1 : Flasherflash2_Timer
		Case 16
			FlashLevel4 = 1 : Flasherflash4_Timer
		Case 17
			FlashLevel6 = 1 : Flasherflash6_Timer
		Case 18
			FlashLevel3 = 1 : Flasherflash3_Timer
		Case 19
			FlashLevel5 = 1 : Flasherflash5_Timer
		Case 20
			FlashLevel1 = 1 : Flasherflash1_Timer
		Case 21
			FlashLevel2 = 1 : Flasherflash2_Timer
		Case 22
			FlashLevel4 = 1 : Flasherflash4_Timer
		Case 23
			FlashLevel6 = 1 : Flasherflash6_Timer
		Case 24
			FlashLevel3 = 1 : Flasherflash3_Timer
		Case 25
			FlashLevel5 = 1 : Flasherflash5_Timer
		Case 26
			FlashLevel1 = 1 : Flasherflash1_Timer
		Case 27
			FlashLevel2 = 1 : Flasherflash2_Timer
		Case 28
			FlashLevel4 = 1 : Flasherflash4_Timer
		Case 29
			FlashLevel6 = 1 : Flasherflash6_Timer
		Case 30
			FlashLevel3 = 1 : Flasherflash3_Timer
			flashseq = 0
			flashflash.Enabled = False
	End Select
End Sub

'	sub magnettime_timer
'		If lfup + rfup = 2 Then
'			MagnetW2.MagnetOn = true
'l18.state = 2
'			playsound"buzz"
'		Else
'			MagnetW2.MagnetOn = False
'l18.state = 0
'		end if
'	End sub

dim fpos: fpos = 0
sub spinflash_timer
	fpos = fpos + 1
	select case fpos
		case 1
			spinflashright.visible = 1
			spinflashleft.visible = 1
		case 2
			spinflashright.visible = 0
			spinflashleft.visible = 0
			fpos = 0
	end Select
end Sub




'################################################################
'####  additional code 
'################################################################

'#### 1. Skillshots

'Dim Lrro_state    'Skillshot light
Sub Skillshottimer_timer
	Skillshottimer.enabled = False
	Endskillshot
	if ModeCycleTimer.enabled Then
		EndMode
	end If	
End Sub

'Skillshot 1 - short plunge, below upper right flipper
Sub sw25_Hit()
	Controller.Switch(25) = 1
	PlaySound "rollover"
	if Skillshotactive then
		if HasPup Then
			PuPlayer.LabelSet pBackglass,"Skillshot1bg","Skillshot!",1,""
			PuPlayer.LabelSet pBackglass,"SkillshotInfo1bg",FormatPointText("10000"),1,""		
		end if
		Addpoints 10000
		SkillshotFlasher
		EndSkillshot
		EndMode
	else 
		if not ModeActiveTimer.enabled then
			NextMode
		Else
			'Award one Mode feature
			CheckLoopMania
			CheckDTFrenzy
			CheckSuperSpinner
			CheckSuperPopBumper
			CheckBeatlemania
			if BeatlemaniaMBTimer.enabled and bip < (2 + Modelevel(PlayerNo)) then
				AddMultiBall
			end If
		end If
	end if
	CheckBeatlemania	
End Sub
Sub sw25_UnHit():Controller.Switch(25) = 0:End Sub

'Skillshot 2 - medium plunge to upper right flipper, then loop
Dim Loop1ok, Loop2ok
Sub LoopSkillshot1_hit
	if Skillshotactive then
		Skillshottimer.enabled = False
		Skillshottimer.enabled = True
	end if
	if Loop1ok Then
		EndSkillshot
	end If
	if ModeCycleTimer.enabled Then
		EndMode
	end If	
End Sub

Sub LoopSkillshot2_hit
	if Loop1ok and Skillshotactive then
		if HasPup Then
			PuPlayer.LabelSet pBackglass,"Skillshot2bg","Skillshot!",1,""
			PuPlayer.LabelSet pBackglass,"SkillshotInfo2bg",FormatPointText("25000"),1,""	
		end if
		Addpoints 25000
		SkillshotFlasher
		EndSkillshot
		EndMode
	end if
	if Skillshotactive then
		Skillshottimer.enabled = False
		Skillshottimer.enabled = True
	end if
	Loop2ok = True
End Sub

Sub LoopSkillshot3_hit
	Loop1ok = True
	ModeCycleTimer.enabled = False
End Sub

'Skillshot 3 - stronger plunge into top lane at magnet
Sub TopSkillshot_Hit
	if Loop2ok and Skillshotactive then
		if HasPup Then
			PuPlayer.LabelSet pBackglass,"Skillshot3bg","Skillshot!",1,""
			PuPlayer.LabelSet pBackglass,"SkillshotInfo3bg",FormatPointText("100000"),1,""	
		end if
		Addpoints 100000
		SkillshotFlasher
		EndSkillshot
		EndMode
	end if
	if Skillshotactive then
		EndSkillshot
	end if
End Sub

Dim Skillshotactive, SkillBallplayed
SkillBallplayed = 0

'new Skillshot on next ball or with new player
Sub StartSkillshot_Hit
	if (Skillballplayed <> (scores(31) + (Playerno * 100))) or ShootAgain then
		Skillballplayed = (scores(31) + (Playerno * 100))
		Skillshotactive = True
		Lrro.state = Lightstateblinking
		Loop1ok = False
		Loop2ok	= False 
		ShootAgain=False
	end if	
End Sub

Sub StartSkillshotTimer_Hit
	if Skillshotactive Then
		Skillshottimer.enabled = True
	end If
End Sub

Sub EndSkillshot1_hit
	if Skillshotactive Then
		Endskillshot
		EndMode
	end If
End Sub

Sub EndSkillshot2_hit
	if Skillshotactive Then
		Endskillshot
		EndMode
	end If
End Sub

Sub Endskillshot
	Skillshottimer.enabled = False
	Skillshotactive = False
	Lrro.state = Lightstateoff
	Loop1ok = False
	Loop2ok = False
	if HasPup Then
		vpmtimer.addtimer 4500, "PuPlayer.LabelSet pBackglass,""Skillshot1bg"","""",1,"""" '"
		vpmtimer.addtimer 4500, "PuPlayer.LabelSet pBackglass,""Skillshot2bg"","""",1,"""" '"
		vpmtimer.addtimer 4500, "PuPlayer.LabelSet pBackglass,""Skillshot3bg"","""",1,"""" '"
		vpmtimer.addtimer 4500, "PuPlayer.LabelSet pBackglass,""SkillshotInfo1bg"","""",1,"""" '"
		vpmtimer.addtimer 4500, "PuPlayer.LabelSet pBackglass,""SkillshotInfo2bg"","""",1,"""" '"
		vpmtimer.addtimer 4500, "PuPlayer.LabelSet pBackglass,""SkillshotInfo3bg"","""",1,"""" '"
	end if
End sub

Sub SkillshotFlasher
	playsound SoundFX("Knocker",DOFKnocker)
	FlashLevel1 = 1 : Flasherflash1_Timer
	FlashLevel2 = 1 : Flasherflash2_Timer
	FlashLevel3 = 1 : Flasherflash3_Timer
	FlashLevel5 = 1 : Flasherflash5_Timer
	FlashLevel7 = 1 : Flasherflash7_Timer
	strip1.visible = 1
	strip3.visible = 1
	strip5.visible = 1
	strip7.visible = 1
	strip9.visible = 1
	f4.visible = 1
	f5.visible = 1
	vpmtimer.addtimer 300, "BackFlasheroff '"
End Sub

'#### 2. Mystery Target

Dim MysteryActive, Mystery1Ready, Mystery2Ready
Randomize

Sub LiteMysteryTarget1_Hit
	Mystery1Ready = True
	LiteMystery1.state = Lightstateon
	if Mystery1Ready and Mystery2Ready Then
		MysteryActive = True
		MysteryLit.state = Lightstateblinking	
	end if
End Sub

Sub LiteMysteryTarget2_Hit
	Mystery2Ready = True
	LiteMystery2.state = Lightstateon
	if Mystery1Ready and Mystery2Ready Then
		MysteryActive = True
		MysteryLit.state = Lightstateblinking	
	end if
End Sub

Sub EndMystery
	Mystery1Ready = False
	Mystery2Ready = False
	LiteMystery1.state = Lightstateoff
	LiteMystery2.state = Lightstateoff
	MysteryLit.state = Lightstateoff
	MysteryActive = False
	MysteryDisplayTimer.enabled = True
End Sub

Sub MysteryDisplayTimer_Timer
	MysteryDisplayTimer.enabled = False
	ClearMysteryDisplay
End Sub

Sub MysteryTarget_Hit
	if MysteryActive then
		ClearMysteryDisplay
		Playsound SoundFX("Knocker",DOFKnocker)
		EndMystery
		if HasPuP Then
			PuPlayer.LabelSet pBackglass,"MysteryInfobg","Mystery Award:",1,""
		end if
		select Case int(6*rnd)	'0-5
			case 0:
				'10 Sec Ballsaver
				if HasPuP Then
					PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","10sec Ballsaver",1,""
				end if
				BSoffTimer.enabled = False
				BSGraceTimer.enabled = False
				MysteryBSoffTimer.enabled = False
				MysteryBSoffTimer.enabled = True	
				ballsave.state = 2
				bspos = 1
				BallsSaved = 0
			case 1:
				'Small Points
				if HasPuP Then
					PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","Small Points",1,""
				end if
				Addpoints 1000
			case 2:
				'Medium Points
				if HasPuP Then
					PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","Medium Points",1,""
				end if
				Addpoints 10000
			case 3:
				'Big Points
				Addpoints 25000
				if HasPuP Then
					PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","Big Points",1,""
				end if
			case 4:
				'Advance Spinner
				if HasPuP Then
					PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","Advance Spinner",1,""
				end if
				if l8.state = Lightstateoff Then
					vpmtimer.pulsesw 20
				Else
					if l24.state = Lightstateoff Then
						vpmtimer.pulsesw 19
					Else
						if l40.state = Lightstateoff Then
							vpmtimer.pulsesw 18
						Else
							if l56.state = Lightstateoff Then
								vpmtimer.pulsesw 17
							Else
								spinflashleft.enabled = True
							end If
						end If
					end If
				end If
			case 5:
				'Advance Mode selection
				if ModeReady > 1 Then
					if ModeActiveTimer.enabled Then
						'Award one Mode feature
						CheckLoopMania
						CheckDTFrenzy
						CheckSuperSpinner
						CheckSuperPopBumper
						CheckBeatlemania
						if BeatlemaniaMBTimer.enabled and bip < (2 + Modelevel(PlayerNo)) then
							AddMultiBall
							if HasPuP Then
								PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","Ball added",1,""
							end if
						Else
							if HasPuP Then
								PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","1 Mode Score",1,""
							end if
						end If
					Else
						if HasPuP Then
							PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","Start Mode",1,""
						end if
						vpmtimer.addtimer 2000,"StartMode '"
					end If
				Else
					NextMode
					if HasPuP Then
						PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","Advance Mode",1,""
					end if
				end If
			case 6:
				'Boundary Check
				if HasPuP Then
					PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","Nothing...",1,""
				end if
		end Select
	end if	
End Sub

Sub ClearMysteryDisplay
	if HasPuP Then
		PuPlayer.LabelSet pBackglass,"MysteryInfobg","",1,""
		PuPlayer.LabelSet pBackglass,"MysteryInfo2bg","",1,""
	end if
End Sub

'#### 3. Turntable activated by Loops and Drop Targets

Dim TurntableCount

Sub TurntableTimer_Timer
	TurntableCount = TurntableCount - 1
	if TurntableCount <= 0 Then
		TurntableTimer.enabled = False
		TurntableCount = 0
		spinner.MotorOn = False
	end If	
End Sub

Sub AddTurntableTime
	if Turntablecount < 4 then
		TurntableCount = TurntableCount + 1
	end if
	if not TurntableTimer.enabled Then
		spinner.MotorOn = True
		TurntableTimer.enabled = True
	end if
End Sub

Dim CurrSpeed
Sub recordspin_timer
	CurrSpeed = spinner.speed * 15 / spinner.maxspeed
	recordplate.RotZ = recordplate.RotZ + CurrSpeed
	if recordplate.RotZ >= 360 then
		recordplate.RotZ = 0
	end if
	if recordplate.RotZ < 0 then
		recordplate.RotZ = 360 + recordplate.RotZ
	end if
End Sub

'#### 4. Modes

' 1st Mode - 2-Ball Multiball
' this is been done by the existing SuperJackpot Ball Lock

' All other modes can be lit and cycled by hitting the Star Rollover switch below the right upper flipper (not during Skillshot)
' The Mystery Target awards lights a Mode too, occasionally
' Shoot Loop to Top Magnet to start the selected Mode, Ball is held and served to the Bumpers
'
' 2nd Mode - Loop Mania
'	50 sec additional 5000 Points per Loop
' 3rd Mode - Drop Target Frenzy
' 	40 sec additional 2000 Points per DT hit
' 4th Mode - Super Spinners
'	30 sec additional 1000 Points per Spin
' 5th Mode - Super Pop Bumpers
'	40 sec additional 1000 Points per Bumper hit
' Final Mode - Beatlemania
'	If all Modes are completed, the Top Magnet will start Beatlemania, 60 sec every switch adds 2000 Points, 10 sec Ballsaver
'
' After completing all 5 modes, they can be retarted at the next level. Scoring is level times score.
' Lights indicate mode level: blue - L1, green - L2, red - L3, yellow - L4

Sub LoopMania1_Hit
	if Loopmania2.timerenabled Then
		CheckLoopMania
		Loopmania2.timerenabled = False
	Else
		Loopmania1.timerenabled = False
		Loopmania1.timerenabled = True
	end If
End Sub

Sub LoopMania1_Timer
	Loopmania1.timerenabled = False
End Sub

Sub LoopMania2_Hit
	if Loopmania1.timerenabled Then
		CheckLoopMania
		Loopmania1.timerenabled = False
	Else
		Loopmania2.timerenabled = False
		Loopmania2.timerenabled = True
	end If
End Sub

Sub LoopMania2_Timer
	Loopmania2.timerenabled = False
End Sub

Dim ModeTotal,Jackpotvalue(5)
Sub CheckLoopMania
	if Mode2Timer.enabled Then
		addpoints 5000 * Modelevel(PlayerNo)
		ModeTotal = ModeTotal + 5000 * Modelevel(PlayerNo)
		playsound "neight"
	end if
	if not NoExploitTimer.enabled then
		Jackpotvalue(PlayerNo) = Jackpotvalue(PlayerNo) + 5000
		if HasPup And not MysteryDisplayTimer.enabled and not NoExploitTimer.enabled then
			PuPlayer.LabelSet pBackglass,"SkillshotInfo1bg","",1,""
			PuPlayer.LabelSet pBackglass,"SkillshotInfo2bg","",1,""
			PuPlayer.LabelSet pBackglass,"SkillshotInfo3bg","",1,""
			PuPlayer.LabelSet pBackglass,"Jackpotbg","Jackpot Value",1,""
			PuPlayer.LabelSet pBackglass,"Jackpot2bg",FormatPointText(cstr(Jackpotvalue(PlayerNo))),1,""
			vpmtimer.addtimer 1800, "PuPlayer.LabelSet pBackglass,""Jackpotbg"","""",1,"""" '"
			vpmtimer.addtimer 1800, "PuPlayer.LabelSet pBackglass,""Jackpot2bg"","""",1,"""" '"
		end If
	end If
End Sub

' 3rd Mode - Drop Target Frenzy
Sub CheckDTFrenzy
	if Mode3Timer.enabled Then
		addpoints 1000 * Modelevel(PlayerNo)
		ModeTotal = ModeTotal + 1000 * Modelevel(PlayerNo)
		playsound "symbol"
	end if
End Sub

Sub SetDTState(IndexPar,DTObjPar,LampObjPar)
	if Mode3Timer.enabled Then
		DTObjPar.DropSol_On
		vpmtimer.addtimer 1000,"LiftDTLBank '"
	Else
		LampObjPar.state = Lightstateon
	end if
End Sub

sub LiftDTLBank
	sw38.isdropped = False:sw39.isdropped = false:sw40.isdropped = false
end Sub

Sub ResetDropTargets
	Playsound SoundFX("DTReset",DOFContactors)
	if not DT_State(0) Then
		sw40.isdropped = False
	end If
	if not DT_State(1) Then
		sw39.isdropped = False
	end If
	if not DT_State(2) Then
		sw38.isdropped = False
	end If
	if not DT_State(3) Then
		sw21.isdropped = False
	end If
	if not DT_State(4) Then
		sw22.isdropped = False
	end If
	if not DT_State(5) Then
		sw23.isdropped = False
	end If
	if not DT_State(6) Then
		sw24.isdropped = False
	end If
	if not DT_State(7) Then
		sw29.isdropped = False
	end If
	if not DT_State(8) Then
		sw30.isdropped = False
	end If
	if not DT_State(9) Then
		sw31.isdropped = False
	end If
	if not DT_State(10) Then
		sw32.isdropped = False
	end If
	checkfab
End Sub

' 4th Mode - Super Spinners
Sub CheckSuperSpinner
	if Mode4Timer.enabled Then
		addpoints 1000 * Modelevel(PlayerNo)
		ModeTotal = ModeTotal + 1000 * Modelevel(PlayerNo)
		playsound "gate"
	end if
End Sub

' 5th Mode - Super Pop Bumpers
Sub CheckSuperPopBumper
	if Mode5Timer.enabled Then
		addpoints 1000 * Modelevel(PlayerNo)
		ModeTotal = ModeTotal + 1000 * Modelevel(PlayerNo)
		playsound "big drum"
	end if
End Sub

Dim BumperState: Bumperstate = 1
Sub FlashPopBumpersTimer_Timer
	SetLamp 180, Bumperstate
	SetLamp 181, Bumperstate
	SetLamp 182, Bumperstate
	Bumperstate = Bumperstate + 1
	if Bumperstate <> 1 Then
		Bumperstate = 0
	end If
End Sub

' Final Mode - Beatlemania
Sub CheckBeatlemania
	if BeatlemaniaTimer.enabled or BeatlemaniaMBTimer.enabled Then
		addpoints 2000 * Modelevel(PlayerNo)
		ModeTotal = ModeTotal + 2000 * Modelevel(PlayerNo)
		playsound "painting"
	end if
End Sub

Sub AddMultiBall
	KickSavedBall = True
	ballrelease.createball:BallRelease.Kick 90, 4
	bip = bip + 1
End Sub


Dim ModeReady:ModeReady = 1
Dim Modecompleted(4,6)

Sub NextMode
	if (not Modecompleted(PlayerNo,2)) or (not Modecompleted(PlayerNo,3)) or (not Modecompleted(PlayerNo,4)) or (not Modecompleted(PlayerNo,5)) then
		if not ModeActiveTimer.enabled then
			ModeReady = Modeready + 1
			if Modeready > 5 Then		
				Modeready = 2
			end if	
			Modecompleted(PlayerNo,0) = True
			if not Modecompleted(PlayerNo,1) then
				Mode1Light.state = Lightstateoff
				Modecompleted(PlayerNo,0) = False
			end If
			if not Modecompleted(PlayerNo,2) then
				Mode2Light.state = Lightstateoff
				Modecompleted(PlayerNo,0) = False
			end If
			if not Modecompleted(PlayerNo,3) then
				Mode3Light.state = Lightstateoff
				Modecompleted(PlayerNo,0) = False
			end If
			if not Modecompleted(PlayerNo,4) then
				Mode4Light.state = Lightstateoff
				Modecompleted(PlayerNo,0) = False
			end If
			if not Modecompleted(PlayerNo,5) then
				Mode5Light.state = Lightstateoff
				Modecompleted(PlayerNo,0) = False
			end If
			if ModeReady > 0 Then
				if Modecompleted(PlayerNo,ModeReady) Then
					NextMode
				Else
					select case ModeReady
						case 2: Mode2Light.state = Lightstateblinking
						case 3: Mode3Light.state = Lightstateblinking
						case 4: Mode4Light.state = Lightstateblinking
						case 5: Mode5Light.state = Lightstateblinking
					end Select
				End If
			end If
		end If
	end if
End Sub

Dim DT_State(10)
Sub StartMode
	If (Modeready > 1) and (not ModeActiveTimer.enabled) Then
		MagnetW2.MagnetOn = False
		l18.state = 0
		select case ModeReady
			case 2: 'Loop Mania
					Mode2Light.state = Lightstateblinking
					Mode2Timer.enabled = True
					ModeTime = 50
					ModeTotal = 0
					l4.color = Modecolor(Modelevel(PlayerNo))
					l4.colorfull = Modecolor(Modelevel(PlayerNo))
					l4.state = Lightstateblinking
					l4b.color = Modecolor(Modelevel(PlayerNo))
					l4b.colorfull = Modecolor(Modelevel(PlayerNo))
					l4b.state = Lightstateblinking
					if HasPup Then
						PuPlayer.LabelSet pBackglass,"ModeInfobg","Loop Mania",1,""
						PuPlayer.playlistplayex pVids,"modes","2_Drive My Car.mp4",videovolume,1
					end if
			case 3: 'Drop Target Frenzy
					Mode3Light.state = Lightstateblinking
					DT_State(0) = Fabf.state
					DT_State(1) = Faba.state
					DT_State(2) = Fabb.state
					DT_State(3) = Fourf.state
					DT_State(4) = Fouro.state
					DT_State(5) = fouru.state
					DT_State(6) = fourr.state
					DT_State(7) = l19641.state
					DT_State(8) = l19649.state
					DT_State(9) = l19646.state
					DT_State(10) = l19644.state
					dtBank.DropSol_On
					dtLBank.DropSol_On
					dtRBank.DropSol_On
					dim a
					For each a in DTlights
						a.State = Lightstateblinking
					Next
					Mode3Timer.enabled = True
					ModeTime = 40
					ModeTotal = 0
					if HasPup Then
						PuPlayer.LabelSet pBackglass,"ModeInfobg","Drop Target Frenzy",1,""
						PuPlayer.playlistplayex pVids,"modes","3_Known Better.mp4",100,1			'volume is too low
					end if
			case 4: Mode4Light.state = Lightstateblinking
					Mode4Timer.enabled = True
					spinflash.enabled = True
					ModeTime = 30
					ModeTotal = 0
					if HasPup Then
						PuPlayer.LabelSet pBackglass,"ModeInfobg","Super Spinners",1,""
						PuPlayer.playlistplayex pVids,"modes","4_ticket_to_ride.mp4",videovolume,1
					end if
			case 5: Mode5Light.state = Lightstateblinking
					Mode5Timer.enabled = True
					FlashPopBumpersTimer.enabled = True
					ModeTime = 40
					ModeTotal = 0	
					if HasPup Then
						PuPlayer.LabelSet pBackglass,"ModeInfobg","Super Pop Bumpers",1,""
						PuPlayer.playlistplayex pVids,"modes","5_It wont be long.mp4",videovolume,1
					end if
			case 6: BeatlemaniaTimer.enabled = True
					ModeTime = 60
					ModeTotal = 0
					if HasPup Then
						PuPlayer.LabelSet pBackglass,"ModeInfobg","Beatle Mania",1,""
						PuPlayer.playlistplayex pVids,"modes","6_Rollover Beethoven.mp4",videovolume,1
					end if
					BSoffTimer.enabled = False
					BSGraceTimer.enabled = False
					MysteryBSoffTimer.enabled = False
					MysteryBSoffTimer.enabled = True	
					ballsave.state = 2
					bspos = 1
					BallsSaved = 0
					if BM_Multiball_Level <= Modelevel((PlayerNo)) Then
						BeatlemaniaTimer.enabled = False
						BeatlemaniaMBTimer.enabled = True
						vpmtimer.addtimer 1500, "AddMultiBall '"
						if ballinlock = 1 Then
							superjlock.isdropped = True
							ballinlock = 0
							bip = bip + 1
							sw38.isdropped = false:sw39.isdropped = false:sw40.isdropped = false
							Playsound SoundFX("DTReset",DOFContactors)
							fabf.state = 0:faba.state = 0:fabb.state = 0
						Else
							vpmtimer.addtimer 3500, "AddMultiBall '"	
						end If
					end if
		end Select
		ModeActiveTimer.enabled = True
		if not BeatlemaniaMBTimer.enabled Then
			ModeCountTimer.enabled = True
		end If
		if HasPup Then
			if not BeatlemaniaMBTimer.enabled Then
				PuPlayer.LabelSet pBackglass,"ModeTimerbg","Get Ready",1,""
			end If
			PuPlayer.LabelSet pBackglass,"ModeLevelbg","Level "&Modelevel(PlayerNo),1,""
		end if
	end if
End Sub

Sub Mode2Timer_Timer:EndMode:End Sub
Sub Mode3Timer_Timer:EndMode:End Sub
Sub Mode4Timer_Timer:EndMode:End Sub
Sub Mode5Timer_Timer:EndMode:End Sub	
Sub BeatlemaniaTimer_Timer:EndMode:End Sub	

Sub BeatlemaniaMPTimer_Timer
	if pupballstart.ballcntover Then
		Plunger.AutoPlunger = true
		Plunger.Pullback
		Plunger.Fire
		PlaySound"plunger"
		Plunger.AutoPlunger = false
		KickSavedBall = False
	end If
End Sub

Dim ModeTime
Sub ModeCountTimer_Timer
	if ModeTime > 0 Then
		modetime = Modetime - 1
		if HasPup Then
			PuPlayer.LabelSet pBackglass,"ModeTimerbg",cstr(ModeTime),1,""
		end if
	Else
		ModeCountTimer.enabled = False
	end if
End Sub

Sub EndMode
	ModeCycleTimer.enabled = False
	if ModeActiveTimer.enabled then
		if HarderModes and (ModeTotal = 0) and (ModeReady < 6) then
			select case ModeReady 
				case 1: Modecompleted(PlayerNo,1) = True
				case 2: l4.color = 8618883
						l4.colorfull = 8618883
						l4.state = Lightstateoff
						l4b.color = 8618883
						l4b.colorfull = 8618883
						l4b.state = Lightstateoff
				case 3: Fabf.state = DT_State(0)
						Faba.state = DT_State(1)
						Fabb.state = DT_State(2)
						Fourf.state =DT_State(3)
						Fouro.state =DT_State(4)
						fouru.state =DT_State(5)
						fourr.state =DT_State(6)
						l19641.state =DT_State(7)
						l19649.state =DT_State(8)
						l19646.state =DT_State(9)
						l19644.state =DT_State(10)
						ResetDropTargets
				case 4: spinflash.enabled = False
				case 5: FlashPopBumpersTimer.enabled = False
						SetLamp 180, 1
						SetLamp 181, 1
						SetLamp 182, 1
			End Select
			ModeReady = 1
		else
			select case ModeReady
				case 1: Modecompleted(PlayerNo,1) = True
				case 2: Modecompleted(PlayerNo,2) = True
						l4.color = 8618883
						l4.colorfull = 8618883
						l4.state = Lightstateoff
						l4b.color = 8618883
						l4b.colorfull = 8618883
						l4b.state = Lightstateoff
				case 3: Modecompleted(PlayerNo,3) = True
						Fabf.state = DT_State(0)
						Faba.state = DT_State(1)
						Fabb.state = DT_State(2)
						Fourf.state =DT_State(3)
						Fouro.state =DT_State(4)
						fouru.state =DT_State(5)
						fourr.state =DT_State(6)
						l19641.state =DT_State(7)
						l19649.state =DT_State(8)
						l19646.state =DT_State(9)
						l19644.state =DT_State(10)
						ResetDropTargets
				case 4: Modecompleted(PlayerNo,4) = True
						spinflash.enabled = False
						if Spinnerbonus Then
							spinflashleft.visible = True
							spinflashright.visible = True
						end If
				case 5: Modecompleted(PlayerNo,5) = True:FlashPopBumpersTimer.enabled = False:SetLamp 180, 1:SetLamp 181, 1:SetLamp 182, 1
				case 6: Modecompleted(PlayerNo,0) = True
						Modelevel(PlayerNo) = Modelevel(PlayerNo) + 1
						if Modelevel(PlayerNo) < 4 then
							Modecompleted(PlayerNo,0) = False
							Modecompleted(PlayerNo,1) = False
							Modecompleted(PlayerNo,2) = False
							Modecompleted(PlayerNo,3) = False
							Modecompleted(PlayerNo,4) = False
							Modecompleted(PlayerNo,5) = False
							ModeReady = 2
							Mode1Light.color = Modecolor(Modelevel(PlayerNo))
							Mode1Light.ColorFull = Modecolor(Modelevel(PlayerNo))
							Mode2Light.Color = Modecolor(Modelevel(PlayerNo))
							Mode2Light.ColorFull = Modecolor(Modelevel(PlayerNo))
							Mode3Light.Color = Modecolor(Modelevel(PlayerNo))
							Mode3Light.ColorFull = Modecolor(Modelevel(PlayerNo))
							Mode4Light.Color = Modecolor(Modelevel(PlayerNo))
							Mode4Light.ColorFull = Modecolor(Modelevel(PlayerNo))
							Mode5Light.Color = Modecolor(Modelevel(PlayerNo))
							Mode5Light.ColorFull = Modecolor(Modelevel(PlayerNo))
						end If
			end Select
		end if	
		Mode2Timer.enabled = False
		Mode3Timer.enabled = False
		Mode4Timer.enabled = False
		Mode5Timer.enabled = False
		BeatlemaniaTimer.enabled = False
		BeatlemaniaMBTimer.enabled = False
		ModeActiveTimer.enabled = False
		ModeCountTimer.enabled = False
		if HasPup Then
			PuPlayer.LabelSet pBackglass,"ModeInfo2bg","Total: "&FormatPointText(cstr(ModeTotal)),1,""			
			vpmtimer.addtimer 3500, "PuPlayer.LabelSet pBackglass,""ModeInfobg"","""",1,"""" '"
			vpmtimer.addtimer 3500, "PuPlayer.LabelSet pBackglass,""ModeInfo2bg"","""",1,"""" '"
			vpmtimer.addtimer 3500, "PuPlayer.LabelSet pBackglass,""ModeLevelbg"","""",1,"""" '"
		end if
	end If

	if HasPup Then
		PuPlayer.LabelSet pBackglass,"ModeTimerbg","",1,""
		PuPlayer.playlistplayex pVids,"backglass","",videovolume,1		
	end if

	Mode1Light.state = Lightstateoff
	Mode2Light.state = Lightstateoff
	Mode3Light.state = Lightstateoff
	Mode4Light.state = Lightstateoff
	Mode5Light.state = Lightstateoff
	Drum.state = Lightstateoff
	if Modecompleted(PlayerNo,1) then 
		Mode1Light.state = Lightstateon
	end If
	if Modecompleted(PlayerNo,2) then 
		Mode2Light.state = Lightstateon
	end If
	if Modecompleted(PlayerNo,3) then 
		Mode3Light.state = Lightstateon
	end If
	if Modecompleted(PlayerNo,4) then
		Mode4Light.state = Lightstateon
	end If
	if Modecompleted(PlayerNo,5) then 
		Mode5Light.state = Lightstateon
	end If
	if Modecompleted(PlayerNo,0) then
		Mode1Light.state = Lightstateon
		Mode2Light.state = Lightstateon
		Mode3Light.state = Lightstateon
		Mode4Light.state = Lightstateon
		Mode5Light.state = Lightstateon
		Drum.state = Lightstateon
	end If
	ModeReady = 1
	'Activate Beatlemania Mode
	if Modecompleted(PlayerNo,1) and Modecompleted(PlayerNo,2) and Modecompleted(PlayerNo,3) and Modecompleted(PlayerNo,4) and Modecompleted(PlayerNo,5) and (not Modecompleted(PlayerNo,0)) then
		ModeReady = 6
		Mode1Light.state = Lightstateblinking
		Mode2Light.state = Lightstateblinking
		Mode3Light.state = Lightstateblinking
		Mode4Light.state = Lightstateblinking
		Mode5Light.state = Lightstateblinking
	end If
End Sub

Dim FlasherLoop: FlasherLoop = 0
Sub ModeActiveTimer_Timer
	flashdrum
	FlasherLoop = FlasherLoop + 1
	if Flasherloop > 5 then	
		strip1.visible = 1
		strip3.visible = 1
		strip5.visible = 1
		strip7.visible = 1
		strip9.visible = 1
		f4.visible = 1
		f5.visible = 1
		vpmtimer.addtimer 450, "BackFlasheroff '"
		FlasherLoop = 0
	end If
End Sub

Sub BackFlasheroff	
	strip1.visible = 0
	strip3.visible = 0
	strip5.visible = 0
	strip7.visible = 0
	strip9.visible = 0
	f4.visible = 0
	f5.visible = 0
End Sub

sub magnettime_timer
	If (Modeready > 1) and (not ModeActiveTimer.enabled) and (not ModeCycleTimer.enabled) and (not NoExploitTimer.enabled) Then
		MagnetW2.MagnetOn = true
		l18.state = 2
		if Ubound(MagnetW2.Balls) >= 0 then
			playsound "buzz"
			vpmtimer.addtimer 1600, "StartMode '"
		end if
	Else
		MagnetW2.MagnetOn = False
		l18.state = 0
	end if
End sub

Sub ResetModes
	Mode1Light.state = Lightstateoff
	Mode2Light.state = Lightstateoff
	Mode3Light.state = Lightstateoff
	Mode4Light.state = Lightstateoff
	Mode5Light.state = Lightstateoff
	Drum.state = Lightstateoff
	Mode2Timer.enabled = False
	Mode3Timer.enabled = False
	Mode4Timer.enabled = False
	Mode5Timer.enabled = False
	BeatlemaniaTimer.enabled = False
	ModeActiveTimer.enabled = False
	Modeready = 1
End Sub

Sub ModeCycleTimer_Timer
	NextMode
End Sub

Sub NoExploitTimer_Timer
	if not noexploit then
		NoExploitTimer.enabled = False
	end if
End Sub

Dim ModeColor, Modelevel(5)
Modecolor = Array(0,16711680,65280,255,65535)

'255 blue   - 16711680
'255 red    - 255
'255 green  - 65280
'255 yellow - 65535


'#### Additional Scoring
'use Star Rollover switch to score additional points 
'keep the ball in play until all the additional scores are registered

AddPointsWall.isdropped = True

Dim PointstoAdd
Sub AddPoints(PointsPar)
	PointstoAdd = Pointstoadd + pointspar
	AddPointsTimer.enabled = True
	AddPointsWall.isdropped = False
End Sub

Sub AddPointsTimer_Timer
	if PointstoAdd >= 5000 Then	
		vpmTimer.PulseSw 25      'Star Rollover 5000 Points
		PointstoAdd = Pointstoadd - 5000
	end If
	if pointstoadd <= 0 Then
		AddPointsTimer.enabled = False
		AddPointsWall.isdropped = True
	Else
		if (pointstoadd < 5000) and AddPointsTrigger.ballcntover Then
			vpmTimer.PulseSw 25
			PointstoAdd = 0
			AddPointsTimer.enabled = False
			AddPointsWall.isdropped = True
		end If
	end If
End Sub
 