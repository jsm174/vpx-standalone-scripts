' **********************
' 2016: directb2s / DOF
' by STAT, stefanaustria
' by arngrim
' **********************



'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.6


'///////////////////////-----Table Options-----///////////////////////

Const VRRoom = 0				' Set to 1 for VR room, 0 for desktop/cabinet

Const Cabinetmode = 0			' Set to 1 for Cabinet Mode

Const GI_Dim = 1 				' Set to 1 toDim GI with flipper current draw, 0 for static GI lights
Const BallsToPlay = 5 			' Set to 3 or 5
Const DesktopModeLights = 1		' Set to 1 for blinking lights on desktop mode graphics, 0 for lights off


'----- Shadow Options -----

Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that behaves like a true shadow!
									'2 = flasher image shadow, but it moves like ninuzzu's


'---------------------
'-  Script by ROSVE  - Score System by STAT
'---------------------

'Additional Code by Dozer, leojreimroc

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName = "captfantastic"


'///////////////////////////////
'Drop Target Special / Lane Adjustment plugs.
'See manual "Feature Operation & Scoring" for
'overview of these settings (Page 7) or reference
'included image in archive.

'1 = Conservative / 0 = Liberal

sadjust = 1 'Special Plug Adjust
ladjust = 1 'Lane Plug Adjust

'///////////////////////////////

 Const NudgeOn = 1
 Dim robject, sadjust, ladjust
 Dim Renudge,RenudgeForce
 Dim roll
 Dim ball,ball1
 Dim GameSeq
 Dim MaxPlayers
 Dim ActivePlayer
 Dim Round
 Dim BallInPlay
 Dim Coins
 Dim Tilt
 Dim FreePlayNum
 Dim DoubleBonusFlag
 Dim CollectL
 Dim HiScore
 Dim TopLights,Targets,DoodleBug,InPlunger
 Dim mhole
 Dim Mem(30)
 Dim Alt10p
 Dim bump1,bump2,bump3,bump4
 Dim NOB
 Dim ft
 Dim mhole1,mhole2
 Dim PRpull,PRdir
 Dim SReels(4)
 Dim VRObj
 Dim rst
 Dim hiscstate
 Dim hisc

 Dim STATScores(4)

Sub captfant_Init()
	LoadEM
	HiScore=""
	On Error Resume Next
	HiScore=Cdbl(LoadValue("captfant","DBHiScore"))
	If HiScore="" Then HiScore=5000
	Coins=0
	GameSeq=0  'Game Over
	If B2SOn Then Controller.B2SSetGameover 1
	GameOverBG.State = 1
	ResetBumperRings
	For i=1 to 3
		SetBumperOff i
	Next
	MaxPlayers=0
	CloseGate
	InPlunger=0
	FLFlash 1, FLMaxLight, 0, 1, 1
	ResetMemory 1
	AltLights
	NOB=0
	Set SReels(1) = ScoreReel1
	Set SReels(2) = ScoreReel2
	Set SReels(3) = ScoreReel3
	Set SReels(4) = ScoreReel4
	DOF 165,1
	anidown = 1

	if HSA1="" then HSA1=25
	if HSA2="" then HSA2=25
	if HSA3="" then HSA3=25
	loadhs
	UpdatePostIt

	If VRRoom = 1 Then
		FlPl1.visible = 1
		FreePlayNum = Int(rnd*9)
		FlGameOver.visible = 1
		FlasherMatch
	End If	
		
End Sub
 
 
 Sub InitGame()
    UpdateBIP
    For i=1 to 3
       SetBumperOff i
    Next
'	ResetDrops true
'    PlaySoundat SoundFXDOF("DropTarget_Up",135,DOFPulse,DOFContactors),BulbFil13

	If B2SOn Then Controller.B2SSetGameover 0
    GameOverBG.State = 0
	If VRRoom = 1 Then
		FlGameOver.visible = 0
		FlasherMatch
	End If	
    CloseGate
    FLSetOff 1,FLMaxLight
    LDT15.State = 1
    LDT16.State = 1
    ResetMemory 0
    'Turn on Shoot Again light if mem(2)=1
    If mem(2)=1 Then FLSetOn 17,17
    AltLights
    AddBonus 1
    FLSetOn 19,20

 End Sub

Dim object	
 
    If ShowDT = True Then
		For each object in DT_Stuff 
		Object.visible = 1 	
		Next
        For each object in Cab_Stuff
        Object.visible = 0
        Next
		If DesktopModeLights = 1 Then	
			for each obj in DT_Lights : Obj.state = LightStateOn : next
			for each obj in DT_LightsBlink : Obj.state = LightStateBlinking : next
		Else
			for each obj in DT_Lights : Obj.state = 0 : next
			for each obj in DT_LightsBlink : Obj.state = 0 : next
		End If
'        CabinetRailLeft.visible = 1:CabinetRailRight.visible = 1
'        Card1.Y = 1856:Card2.Y = 1856
	End If
	
	If ShowDt = False Then
		For each object in DT_Stuff 
		Object.visible = 0 	
		Next
        For each object in Cab_Stuff
        Object.visible = 1
        Next
'        CabinetRailLeft.visible = 0:CabinetRailRight.visible = 0
        'LDB.visible = 0
	End If
    
    If BallsToPlay = 3 Then
    Card2.image = "ic3"
    Else
    Card2.image = "ic5"
    End If

Dim BOOT1,BOOT2,BOOT3,BOOT4, REPA(10), REPB(10), REPC(10)

Sub AddScoresToPlayer(s,p)		' * Score System by STAT
	If s = 10 Then
		PlaySound SoundFXDOF("10a",141,DOFPulse,DOFChimes)
	ElseIf s = 100 Then
		PlaySound SoundFXDOF("100a",142,DOFPulse,DOFChimes)
	ElseIf s = 1000 Then
		PlaySound SoundFXDOF("1000a",143,DOFPulse,DOFChimes)
	End If
	STATScores(p) = STATScores(p) + s
    If B2SOn Then
	Controller.B2SSetScorePlayer p, STATScores(p)
    End If
	If VRRoom > 0 Then
		EMMODE = 1
			UpdateReels 1,1 ,STATScores(1), 0, -1,-1,-1,-1,-1
			UpdateReels 2,2 ,STATScores(2), 0, -1,-1,-1,-1,-1
			UpdateReels 3,3 ,STATScores(3), 0, -1,-1,-1,-1,-1
			UpdateReels 4,4 ,STATScores(4), 0, -1,-1,-1,-1,-1
		EMMODE = 0 ' restore EM mode
	End If
     'Handle Score based replays.
     If STATScores(p) > 70000 AND repa(p) = 0 Then
        ReplayAction
        repa(p) = 1
     End If

     If STATScores(p) > 99000 AND repb(p) = 0 Then
        ReplayAction
        repb(p) = 1
     End If

     If STATScores(p) > 131000 AND repc(p) = 0 Then
        ReplayAction
        repc(p) = 1
     End If

	If STATScores(p) > 99999 Then
        If B2SOn Then
			Controller.B2SSetData 24+p,1
        End If
        If p=1 AND boot1 = 0 Then
			PlaySound "Buzzer"
			OTT1.State = 1
			boot1 = 1
			If VRRoom = 1 Then
				Flasher100k
			End If
        End If
        If p = 2 AND boot2 = 0 Then
			PlaySound "Buzzer"
			OTT2.State = 1
			boot2 = 1
			If VRRoom = 1 Then
				Flasher100k
			End If
        End If
        If p=3 AND boot3 = 0 Then
			PlaySound "Buzzer"
			OTT3.State = 1
			boot3 = 1
			If VRRoom = 1 Then
				Flasher100k
			End If
        End If
        If p=4 AND boot4 = 0 Then
			PlaySound "Buzzer"
			OTT4.State = 1
			boot4 = 1
			If VRRoom = 1 Then
				Flasher100k
			End If
        End If
	    
    End if
   If GameSeq = 1 Then
    SReels(p).addvalue(s)
   End If
End Sub
 

 Sub captfant_KeyDown(ByVal keycode)
	If keycode = AddCreditKey Then
		If Coins < 9 Then
			Coins=Coins+1
			If VRRoom = 1 Then
				cred =reels(4, 0)
				reels(4, 0) = 0
				SetDrum -1,0,  0

				SetReel 0,-1,  Coins
				reels(4, 0) = Coins
			End If
		If B2SOn Then Controller.B2SSetCredits Coins
		End If
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
      If keycode = 5 Then
         If Coins < 9 Then
		   Coins=Coins+1
			If VRRoom = 1 Then
				cred =reels(4, 0)
				reels(4, 0) = 0
				SetDrum -1,0,  0

				SetReel 0,-1,  Coins
				reels(4, 0) = Coins
			End If
		If B2SOn Then Controller.B2SSetCredits Coins
		End If
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
     If keycode = StartGameKey Then
		SoundStartButton
         If MaxPlayers < 4 And Coins>0 And Not HSEnterMode=true Then
		   MaxPlayers=MaxPlayers+1
            AddPlayer
        End If    
'		resettimer.enabled=true
	End If

	If HSEnterMode Then HighScoreProcessKey(keycode)

    If KeyCode = LeftMagnaSave Then
    If anidown = 1 Then 
    anidir = 1:IC_Ani.enabled = 1:PlaySound "TargetDrop"
    Else
    anidir = 2:IC_Ani.enabled = 1:PlaySound "TargetDrop"
    End If
    End If

	If keycode = PlungerKey Then
		Plunger.PullBack
         SoundPlungerPull
         PRdir=0
         PRLight.TimerInterval=60
         PRLight.TimerEnabled=true
		If VRRoom > 0 Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

	If keycode = LeftFlipperKey Then
         If Tilt<3 And Round>0 Then
			LFPress = 1
			LF2Press = 1
			LF.fire
			LF2.fire
			If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
				RandomSoundReflipUpLeft LeftFlipper
			Else 
				SoundFlipperUpAttackLeft LeftFlipper
				RandomSoundFlipperUpLeft LeftFlipper
			End If	
			FlipperActivate LeftFlipper, LFPress
			FlipperActivate LeftFlipper2, LF2Press
			LeftFlipperMirror1.RotateToEnd
			LeftFlipperMirror2.RotateToEnd
         End If
		DOF 101, DOFOn
        DOF 166,1:DOF 165,0
        If Gi_Dim = 1 AND GameSeq=1 AND Tilt<3 Then
        For Each gxx in GI_Lights
        gxx.IntensityScale = 0.95
        Next
        gi_bright.enabled = 1
		PlaySoundAtVolLoops "buzzL",LeftFlipper,0.05,-1
        End If
		If VRRoom = 1 Then
			Primary_flipper_button_left.x = Primary_flipper_button_left.x + 4
		End If
	End If
    
	If keycode = RightFlipperKey Then
         If Tilt<3 And Round>0 Then
			RFPress = 1
			RF.fire 'Flipper3.RotateToEnd
			Flipper5.RotateToEnd
			If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
				RandomSoundReflipUpRight RightFlipper
			Else 
				SoundFlipperUpAttackRight RightFlipper
				RandomSoundFlipperUpRight RightFlipper
			End If
			FlipperActivate RightFlipper, RFPress
			RightFlipperMirror.RotateToEnd
			RightFlipperMirror2.RotateToEnd
         End If
		DOF 102, DOFOn
        DOF 166,1:DOF 165,0
         If Gi_Dim = 1 AND GameSeq=1 AND Tilt<3 Then
        For Each gxx in GI_Lights
        gxx.IntensityScale = 0.95
        Next
        gi_bright.enabled = 1
		PlaySoundAtVolLoops "buzz",LeftFlipper,0.05,-1	
        End If
		If VRRoom = 1 Then
			Primary_flipper_button_right.x = Primary_flipper_button_right.x - 4
		End If
	End If
   
    If keycode = MechanicalTilt Then
    Tilt = 3
    TiltOn
    PlaySound "buzzer"
    End If
    
	If keycode = LeftTiltKey Then
      SoundNudgeLeft
      If BallInPlay=1 Then
       If Tilt<3 Then

         Nudge 90, 2
        
          Tilt2Timer.Enabled=1
         If Round>0 Then
           Tilt=Tilt+0.6
           If Tilt>2.9 Then
              TiltOn
           Else
             TiltTimer.Enabled=1
           End If
         End If
       End If
      End If
	End If
    
	If keycode = RightTiltKey Then
      SoundNudgeRight
      If BallInPlay=1 Then
       If Tilt<3 Then
 
		Nudge 270, 2

          Tilt2Timer.Enabled=1
         If Round>0 Then
           Tilt=Tilt+0.6
           If Tilt>2.9 Then
              TiltOn
           Else
             TiltTimer.Enabled=1
           End If
         End If
       End If
      End If
	End If  
 
 	If keycode = CenterTiltKey Then
      SoundNudgeCenter
      If BallInPlay=1 Then
       If Tilt<3 Then
		'Renudge=160+(Rnd*40)
          Tilt2Timer.Enabled=1
        'If NudgeOn=1 Then
		 '  Nudge -Renudge, 1.3
          ' RenudgeForce=1.0
        'Else
         '  Nudge -Renudge,0.9
          ' RenudgeForce=0.4
        'End If

         Nudge 0, 2

         If Round>0 Then
           Tilt=Tilt+0.6
           If Tilt>2.9 Then
              TiltOn
           Else
             TiltTimer.Enabled=1
           End If
         End If
       End If
      End If
	End If
    
End Sub

Sub captfant_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.Fire
		If InPlunger = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
        PRLight.TimerInterval=10
        PRdir=1
        If InPlunger=1 Then
           PlaySound "Shoot"
        End If
	If VRRoom > 0 Then
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If
	End If
	End If
  
	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
		LeftFlipper2.RotateToStart
		LeftFlipperMirror1.RotateToStart
		LeftFlipperMirror2.RotateToStart
		FlipperDeActivate LeftFlipper, LFPress
		FlipperDeActivate LeftFlipper2, LF2Press
		If Tilt<3 And Round>0 Then  
			PlaySoundat SoundFXDOF("SSFlipper2",101,DOFOff,DOFFlippers), Leftflipper
		End If
        StopSound "buzzL"
		If VRRoom = 1 Then
			Primary_flipper_button_left.x = 2104.542
		End If
	End If
    
	If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
		Flipper5.RotateToStart
		RightFlipperMirror.RotateToStart
		RightFlipperMirror2.RotateToStart
		FlipperDeActivate RightFlipper, RFPress
		If Tilt<3 And Round>0 Then  
			PlaySoundat SoundFXDOF("SSFlipper2",102,DOFOff,DOFFlippers), Rightflipper
		End If
        StopSound "buzz"
		If VRRoom = 1 Then
			Primary_flipper_button_right.x = 2108.725
		End If
	End If

End Sub

Sub Drain_Hit()
     RandomSoundDrain drain
	 DOF 133, DOFPulse
     BallInPlay=0
	 Drain.DestroyBall
     If Tilt<3 Then
     GameSeq=3 'Collect Bonus then start new ball
     Else
     GameSeq=2
     End If
     SeqTimer.Enabled=1
     SeqTimer.Interval=250
End Sub

Sub ShooterLane_Hit()
	DOF 134, DOFPulse
End Sub
 
 '----------------------------------------------------------
 '     TILT
 '-----------------------------------------------------------
 
 Sub TiltTimer_Timer()
    TiltTimer.Enabled=0
    If Tilt<3 Then
      Tilt=0
    End If
 End Sub
 
  Sub Tilt2Timer_Timer()
     Tilt2Timer.Enabled=0
     Nudge renudge,RenudgeForce
  End Sub
 
 Sub TiltOn()
	
	If B2SOn Then Controller.B2SSetTilt 1
	If VRRoom = 1 Then
		FlTilt.visible = 1
	End If
    PlaySound SoundFX("solon",0)
    For i=1 to 3
       SetBumperOff i
    Next
    CloseGate
    Bumper1.force = 0:Bumper2.Force = 0:Bumper3.Force = 0
    LeftSlingShot.SlingshotThreshold = 100:RightSlingShot.SlingshotThreshold = 100
    For each Object in Bonus_Lights
    object.state = 0
    next
    If B2Son Then
    PlaySoundat SoundFXDOF("SSFlipper2",101,DOFOff,DOFFlippers), Leftflipper
    PlaySoundat SoundFXDOF("SSFlipper2",102,DOFOff,DOFFlippers), Rightflipper
     LeftFlipper.RotateToStart
     LeftFlipper2.RotateToStart
     RightFlipper.RotateToStart
     Flipper5.RotateToStart
    Else
    PlaySoundat SoundFX("SSFlipper2",DOFFlippers), Leftflipper
    PlaySoundat SoundFX("SSFlipper2",DOFFlippers), Rightflipper
     LeftFlipper.RotateToStart
     LeftFlipper2.RotateToStart
     RightFlipper.RotateToStart
     Flipper5.RotateToStart
    'DOF 101,0
    'DOF 102,0
     End If
 End Sub
 
 '----------------------------------------
  'Add Players and start game
 '----------------------------------------
 Sub AddPlayer()
   Dim sr
  If Round=0 Or Round=3 Or Round=5 Then
    If Coins>0 Then
     Coins=Coins-1
	 If VRRoom = 1 Then
     cred =reels(4, 0)
	 reels(4, 0) = 0
	 SetDrum -1,0,  0

	 SetReel 0,-1,  Coins
	 reels(4, 0) = Coins
	 End If
	If B2SOn Then
'		Controller.B2SSetData 28,Coins
		Controller.B2SSetCredits Coins
		Controller.B2SSetCanPlay MaxPlayers
		Controller.B2SSetMatch 0
		Controller.B2SSetGameover 0
		STATScores(1) = 0
		STATScores(2) = 0
		STATScores(3) = 0
		STATScores(4) = 0
		Controller.B2SSetScorePlayer1 0
		Controller.B2SSetScorePlayer2 0
		Controller.B2SSetScorePlayer3 0
		Controller.B2SSetScorePlayer4 0
		Controller.B2SSetData 150,0:BOOT1=0:OTT1.STATE = 0 
		Controller.B2SSetData 151,0:BOOT2=0:OTT2.STATE = 0
		Controller.B2SSetData 152,0:BOOT3=0:OTT3.STATE = 0
		Controller.B2SSetData 153,0:BOOT4=0:OTT4.STATE = 0
	End If
        BOOT1=0:OTT1.STATE = 0
        BOOT2=0:OTT2.STATE = 0
		BOOT3=0:OTT3.STATE = 0
        BOOT4=0:OTT4.STATE = 0
        GameOverBG.State = 0
        MatchBG.State = 0
        TextBox2.text = ""
		If VRRoom = 1 Then
			FlGameOver.visible = 0
			FlasherMatch
			FlasherPlayers
		End If
    if MaxPlayers = 1 Then
         Round=BallsToPlay  
         ActivePlayer=0
		rst=0
		resettimer.enabled = true
		BGStartTimer.enabled = true
     End If
	If MaxPlayers = 1 Then
		PlaySound "Startup"
	Else
		PlaySound "Bally-addplayer"
	End If
     End If 
   End If    
 End Sub
 
 
 '------------------------------------------------------------------
 '    Game Sequence
 '------------------------------------------------------------------
 Sub SeqTimer_Timer()
    Select Case GameSeq
       Case 0 'Game Over
          ' Attract Mode
       Case 1 'New Ball
              SeqTimer.Enabled=0
              If LightShootAgain.State=0 Then InitGame
              LightShootAgain.State=0
			  If B2SOn Then Controller.B2SSetShootAgain 0
              Set ball = Kicker2.CreateBall
			  DOF 109, DOFPulse
              BallInPlay=1
              Kicker2.Kick 45,14
              RandomSoundBallRelease kicker2
				If VRRoom = 1 Then
					FlSPSA.visible = 0
					Flasherballs
				End If
       Case 2 'Drain
              'Prepare for next ball
			  If B2SOn Then Controller.B2SSetTilt 0
				If VRRoom = 1 Then
					FlTilt.visible = 0
				End If
              Bumper1.force = 10:Bumper2.Force = 10:Bumper3.Force = 10
              LeftSlingShot.SlingshotThreshold = 4:RightSlingShot.SlingshotThreshold = 4
              Tilt=0
              IF mem(2)=0 Then
                 If ActivePlayer=MaxPlayers Then
      
                    If Light1_X17.State = 1 Then
                    Round = Round 
                    Else
                    Round=Round-1
                    End If

                    If Round<1 Then
                       ActivePlayer=0
                    Else
                       ActivePlayer=1
                    End If
                 Else
                    If Light1_X17.State = 1 Then
                    ActivePlayer = ActivePlayer
                    Else   
                    ActivePlayer=ActivePlayer+1
                    End If
                 End If

				If B2SOn Then
					Controller.B2SSetPlayerUp ActivePlayer
				End if
					If VRRoom = 1 Then
						FlasherCurrentPlayer
					End If
 
              End If
              IF Round <1 Then
                 'End of game
				   FreePlayNum = Int(rnd*9)
					If B2SOn Then
						Controller.B2SSetCanPlay 0
						Controller.B2SSetMatch FreePlayNum+1
						Controller.B2SSetGameover 1
						Controller.B2SSetBallinPlay 0
						Controller.B2SSetScoreRolloverPlayer1 0
						Controller.B2SSetScoreRolloverPlayer2 0
						Controller.B2SSetScoreRolloverPlayer3 0
						Controller.B2SSetScoreRolloverPlayer4 0
					End if
					If VRRoom = 1 Then
						FlasherPlayers
						FlasherMatch
						FlGameOver.visible = 1
						FlasherCurrentPlayer
						FlasherBalls
					End If
					hiscstate = 0
                    GameOverBG.State = 1
                    REPA(1) = 0:REPA(2) = 0:REPA(3) = 0:REPA(4) = 0
					REPB(1) = 0:REPB(2) = 0:REPB(3) = 0:REPB(4) = 0
					REPC(1) = 0:REPC(2) = 0:REPC(3) = 0:REPC(4) = 0
                    MatchBG.State = 1
                    TextBox2.text = FreePlayNum * 10
                   ' Check Match Wheel ------
                   For i=1 to MaxPlayers
                     If FreePlayNum = (STATScores(i) mod 10) Then
                        ReplayAction
                     End If
                  Next
                   '--------------------------
                  PlaySound "MekSound"
                  SeqTimer.Enabled=0
                  For i=1 to 3
                     SetBumperOff i
                  Next
                  'Hiscore
                  For i=1 To MaxPlayers
                     HStemp=STATScores(i)
                     If HStemp>HiScore Then
                        HiScore=HStemp
						hiscstate=1
                        SaveValue  "captfant","DBHiScore",HiScore
                     End If
                  Next
					if hiscstate=1 then 
						HighScoreEntryInit()
						HStimer.uservalue = 0
						HStimer.enabled=1
					end if
					UpdatePostIt 
					savehs
                  MaxPlayers=0
              Else
                 'Start new ball
				 ResetDrops true
                 GameSeq=1
                 SeqTimer.Enabled=1
                 SeqTimer.Interval=300
                 If ft=1 Then
                    ft=0
                    PlaySound "SSResetGame"
                    SeqTimer.Interval=1200
                 Else
					If ActivePlayer = 1 and Round = 5 Then
					Else
						PlayStartBall.enabled=true
					End If
                 End If
				If VRRoom = 1 Then
					FlasherBalls
				End If
              End If
       Case 3 'Collect Bonus
                  BCC=0
                  SeqTimer.Enabled=False
                  CollectBonusTimer.Enabled = True
       End Select
 End Sub


sub resettimer_timer
    rst=rst+1
	if rst>1 and rst<12 then
		ResetReelsToZero(1)
		ResetReelsToZero(2)
	end if

    if rst=16 then
    end if
    if rst=18 then
	GameSeq=2
	SeqTimer.Enabled=1
	SeqTimer.Interval=300
    resettimer.enabled=false
    end if
end sub



Sub ResetReelsToZero(reelzeroflag)
	dim d1(5)
	dim d2(5)
	dim scorestring1, scorestring2

	If reelzeroflag=1 then
		scorestring1=CStr(STATScores(1))
		scorestring2=CStr(STATScores(2))
		scorestring1=right("00000" & scorestring1,5)
		scorestring2=right("00000" & scorestring2,5)
		for i=0 to 4
			d1(i)=CInt(mid(scorestring1,i+1,1))
			d2(i)=CInt(mid(scorestring2,i+1,1))
		next
		for i=0 to 4
			if d1(i)>0 then 
				d1(i)=d1(i)+1
				if d1(i)>9 then d1(i)=0
			end if
			if d2(i)>0 then 
				d2(i)=d2(i)+1
				if d2(i)>9 then d2(i)=0
			end if

		next
		STATScores(1)=(d1(0)*10000) + (d1(1)*1000) + (d1(2)*100) + (d1(3)*10) + d1(4)
		STATScores(2)=(d2(0)*10000) + (d2(1)*1000) + (d2(2)*100) + (d2(3)*10) + d2(4)
		If VRRoom > 0 Then
			EMMODE = 1
			UpdateReels 0,0 ,STATScores(0), 0, -1,-1,-1,-1,-1
			UpdateReels 1,1 ,STATScores(1), 0, -1,-1,-1,-1,-1
			UpdateReels 2,2 ,STATScores(2), 0, -1,-1,-1,-1,-1
			UpdateReels 3,3 ,STATScores(3), 0, -1,-1,-1,-1,-1
			UpdateReels 4,4 ,STATScores(4), 0, -1,-1,-1,-1,-1
			EMMODE = 0 ' restore EM mode	
		End If
		If B2SOn Then
			Controller.B2SSetScorePlayer 1, STATScores(1)
			Controller.B2SSetScorePlayer 2, STATScores(2)
		End If
		SReels(1).SetValue(STATScores(1))
		SReels(2).SetValue(STATScores(2))

		scorestring1=CStr(STATScores(3))
		scorestring2=CStr(STATScores(4))
		scorestring1=right("00000" & scorestring1,5)
		scorestring2=right("00000" & scorestring2,5)
		for i=0 to 4
			d1(i)=CInt(mid(scorestring1,i+1,1))
			d2(i)=CInt(mid(scorestring2,i+1,1))
		next
		for i=0 to 4
			if d1(i)>0 then 
				d1(i)=d1(i)+1
				if d1(i)>9 then d1(i)=0
			end if
			if d2(i)>0 then 
				d2(i)=d2(i)+1
				if d2(i)>9 then d2(i)=0
			end if

		next
		STATScores(3)=(d1(0)*10000) + (d1(1)*1000) + (d1(2)*100) + (d1(3)*10) + d1(4)
		STATScores(4)=(d2(0)*10000) + (d2(1)*1000) + (d2(2)*100) + (d2(3)*10) + d2(4)
		If VRRoom > 0 Then
			EMMODE = 1
			UpdateReels 0,0 ,STATScores(0), 0, -1,-1,-1,-1,-1
			UpdateReels 1,1 ,STATScores(1), 0, -1,-1,-1,-1,-1
			UpdateReels 2,2 ,STATScores(2), 0, -1,-1,-1,-1,-1
			UpdateReels 3,3 ,STATScores(3), 0, -1,-1,-1,-1,-1
			UpdateReels 4,4 ,STATScores(4), 0, -1,-1,-1,-1,-1
			EMMODE = 0 ' restore EM mode	
		End If
		If B2SOn Then
			Controller.B2SSetScorePlayer 3, STATScores(3)
			Controller.B2SSetScorePlayer 4, STATScores(4)
		End If
		SReels(3).SetValue(STATScores(3))
		SReels(4).SetValue(STATScores(4))
	end if
end sub

Sub PlayStartBall_timer()
	PlayStartBall.enabled=false
	PlaySound("StartBall2-5")
end sub
 
 '--------------------------------------------------------------------
 '------- TARGETS 
 '--------------------------------------------------------------------
Sub T11_Hit()
   If Tilt>2 Then Exit Sub
	DOF 125, DOFPulse
'   PlaySoundat SoundFXDOF("TargetSound",125,DOFPulse,DOFContactors),T11
	AddScoresToPlayer 100,ActivePlayer
End Sub
Sub T11_Timer()
   T11.TimerEnabled=False
   T12.IsDropped=True
   T11.IsDropped=False
End Sub

Sub T21_Hit()
   If Tilt>2 Then Exit Sub
'   PlaySoundat SoundFXDOF("TargetSound",126,DOFPulse,DOFContactors),T21
	DOF 126, DOFPulse
	AddScoresToPlayer 100,ActivePlayer
End Sub
Sub T21_Timer()
   T21.TimerEnabled=False
   T22.IsDropped=True
   T21.IsDropped=False
End Sub

Sub SW1_Hit()
	If Tilt>2 Then Exit Sub
'	PlaySoundat SoundFXDOF("TargetDrop",127,DOFPulse,DOFContactors),SW1p
	DTHit 1
	DTGI.Intensity = DTGI.Intensity + 1
	DOF 127, DOFPulse
	AddScoresToPlayer 500,ActivePlayer
	DTargs.enabled = 1
End Sub
Sub SW2_Hit()
	If Tilt>2 Then Exit Sub
'	PlaySoundat SoundFXDOF("TargetDrop",127,DOFPulse,DOFContactors),SW2p
	DTHit 2
	DOF 127, DOFPulse
	AddScoresToPlayer 500,ActivePlayer
	DTargs.enabled = 1
End Sub
Sub SW3_Hit()
	If Tilt>2 Then Exit Sub
	DTGI.Intensity = DTGI.Intensity + 1
'	PlaySoundat SoundFXDOF("TargetDrop",127,DOFPulse,DOFContactors),SW3p
	DTHit 3
	DOF 127, DOFPulse
	AddScoresToPlayer 500,ActivePlayer
   DTargs.enabled = 1
End Sub
Sub SW4_Hit()
	If Tilt>2 Then Exit Sub
	DTGI.Intensity = DTGI.Intensity + 1
'	PlaySoundat SoundFXDOF("TargetDrop",127,DOFPulse,DOFContactors),SW4p
	DTHit 4
	DOF 127, DOFPulse
	AddScoresToPlayer 500,ActivePlayer
    DTargs.enabled = 1
End Sub
Sub SW5_Hit()
	If Tilt>2 Then Exit Sub
	DTGI.Intensity = DTGI.Intensity + 1
'	PlaySoundat SoundFXDOF("TargetDrop",127,DOFPulse,DOFContactors),SW5p
	DTHit 5
	DOF 127, DOFPulse
	AddScoresToPlayer 500,ActivePlayer
	DTargs.enabled = 1
End Sub

Dim gxx

Sub GI_Bright_Timer()
For Each gxx in GI_Lights
    gxx.Intensityscale = 1.1
    Next
    DOF 166,0:DOF 165,1
    me.enabled = 0
End Sub

 '----------------------------------------------------------------------
 '     BUMPER
 '----------------------------------------------------------------------
Sub Bumper1_Hit()
	If Tilt>2 Then Exit Sub
	If mem(10)=0 Then
		AddScoresToPlayer 10,ActivePlayer
		AltLights
	Else
		AddScoresToPlayer 100,ActivePlayer
	End If
	RandomSoundBumperTop Bumper1
	DOF 103, DOFPulse
End Sub

Sub Bumper2_Hit()
	If Tilt>2 Then Exit Sub
	If mem(10)=0 Then
		AddScoresToPlayer 10,ActivePlayer
		AltLights
	Else
		AddScoresToPlayer 100,ActivePlayer
	End If
	RandomSoundBumperMiddle Bumper2
	DOF 104, DOFPulse
End Sub

Sub Bumper3_Hit()
	If Tilt>2 Then Exit Sub
	If mem(11)=0 Then
		AddScoresToPlayer 10,ActivePlayer
		AltLights
	Else
		AddScoresToPlayer 100,ActivePlayer
	End If
	RandomSoundBumperBottom bumper3
	DOF 105, DOFPulse
End Sub

 Sub SetBumperOn(ByVal bnr)
   Dim a,i
   a=(bnr-1)*3
   For i=a To a+2
   Next
   a=(bnr-1)*2
   For i=a To a+1
     Bumpers(i).State=1
   Next
End Sub

Sub SetBumperOff(ByVal bnr)
   Dim a,i
   a=(bnr-1)*3
   For i=a To a+2
   Next
   a=(bnr-1)*2
   For i=a To a+1
     Bumpers(i).State=0
   Next
End Sub

Sub ResetBumperRings()
End Sub

 '------------------------------------------------------------------------
 '           TRIGGERS
 '------------------------------------------------------------------------
Sub Trigger1_Hit()
    PlaySoundAtBall "sensor"
	DOF 111, DOFPulse
    Trigger1.TimerEnabled=True
    If Tilt>2 Then Exit Sub
		AddScoresToPlayer 100,ActivePlayer
    
    If mem(16)>1 And mem(18)=0 AND Light1_X14.state = 1 Then
       FLSetOff 14,14
       mem(18)=1
       ReplayAction
    End If

   If mem(16)>0 And mem(18)=0 AND Light1_X14.state = 1 AND sadjust = 0 Then
       FLSetOff 14,14
       mem(18)=1
       ReplayAction
    End If

End Sub
Sub Trigger1_Timer()
    Trigger1.TimerEnabled=False
End Sub

Sub Trigger2_Hit()
    PlaySoundAtBall "sensor"
	DOF 112, DOFPulse
     Trigger2.TimerEnabled=True
    If Tilt>2 Then Exit Sub
		AddScoresToPlayer 300,ActivePlayer
    AddBonus 3
    If mem(16)>0 And mem(17)=0 AND Light1_X15.State = 1 Then
       FLSetOff 15,15
       FLSetOn 17,17
       mem(17)=1
       If B2SOn Then Controller.B2SSetShootAgain 1
		If VRRoom = 1 Then
			FlSPSA.visible = 1
		End If
    End If
End Sub
Sub Trigger2_Timer()
    Trigger2.TimerEnabled=False
End Sub

Sub Trigger3_Hit()
    PlaySoundAtBall "sensor"
	DOF 113, DOFPulse
    Trigger3.TimerEnabled=True
    If Tilt>2 Then Exit Sub
		AddScoresToPlayer 100,ActivePlayer
    If mem(6) =1 Then
       AddBonus 3
		AddScoresToPlayer 200,ActivePlayer
       Flipper1.TimerEnabled = True
    End If
End Sub
Sub Trigger3_Timer()
    Trigger3.TimerEnabled=False
End Sub
Sub Flipper1_Timer()
   Flipper1.TimerEnabled = False
   CloseGate
End Sub

Sub Trigger4_Hit()
    PlaySoundAtBall "sensor"
	DOF 114, DOFPulse
    Trigger4.TimerEnabled=True
    If Tilt>2 Then Exit Sub
    If mem(13)=0 Then
      mem(13)=1
      FLSetOff 19,19
      FLSetOn 12,12
      If mem(14)=1 Then
         FLSetOn 11,11
         mem(12)=1
      End If
    End If
    If mem(1)=1 Then
       AddBonus 3
       AddScoresToPlayer 300,ActivePlayer
       Else
       AddScoresToPlayer 100,ActivePlayer
    End If
End Sub
Sub Trigger4_Timer()
    Trigger4.TimerEnabled=False
End Sub

Sub Trigger5_Hit()
    PlaySoundAtBall "sensor"
	DOF 115, DOFPulse
    Trigger5.TimerEnabled=True
    If Tilt>2 Then Exit Sub
		AddScoresToPlayer 100,ActivePlayer
    If mem(1)=0 Then
       OpenGate
    End If
End Sub
Sub Trigger5_Timer()
    Trigger5.TimerEnabled=False
End Sub

Sub Trigger6_Hit()
    PlaySoundAtBall "sensor"
	DOF 116, DOFPulse
    Trigger6.TimerEnabled=True
    If Tilt>2 Then Exit Sub
    If mem(14)=0 Then
      mem(14)=1
      FLSetOff 20,20
      FLSetOn 13,13
      If mem(13)=1 Then
         FLSetOn 11,11
         mem(12)=1
      End If
    End If
    If mem(1)=1 Then
       AddBonus 3
       AddScoresToPlayer 300,ActivePlayer
       Else
       AddScoresToPlayer 100,ActivePlayer
    End If
End Sub
Sub Trigger6_Timer()
    Trigger6.TimerEnabled=False
End Sub


Sub Btriggs_Hit(idx)
PlaySoundatBall "sensor"
If Tilt>2 Then Exit Sub
    If mem(1)=1 Then
       AddBonus 1
    End If
If mem(1)=1 Then
    AddBonus 1
    End If
If BallsToPlay=3 Then
AddScoresToPlayer 1000,ActivePlayer
Else
AddScoresToPlayer 100,ActivePlayer
End If
End Sub


Sub Trigger13_Hit()
    PlaySoundAtBall "sensor"
    'ROlights 7,1
    Trigger13.TimerEnabled=True
    If Tilt>2 Then Exit Sub
     AltLights
     AddScoresToPlayer 10, ActivePlayer
End Sub
Sub Trigger13_Timer()
    Trigger13.TimerEnabled=False
End Sub

Sub Trigger14_Hit()
    PlaySoundAtBall "sensor"
    Trigger14.TimerEnabled=True
    If Tilt>2 Then Exit Sub
     AltLights
     AddScoresToPlayer 10, ActivePlayer
End Sub
Sub Trigger14_Timer()
    Trigger14.TimerEnabled=False
    ROlights 8,0
End Sub


Sub ROlights(Byval nr, Byval st)
   Select Case nr
      Case 1,2,3,4,5,6
         If mem(1)=0 Then

         Else

         End If
      Case 7,8
         If mem(1)=1 Then

         Else

         End If
      End Select
End Sub

'-----
Sub TriggerPlunger_Hit()
    InPlunger=1
End Sub

Sub TriggerPlunger_Unhit()
    InPlunger=0
    ' Shoot Again Off
    mem(2)=0
    FLSetOff 30,30
End Sub

 Sub TriggerNudge1_Hit()
    NudgeOn=0
  End Sub
 Sub TriggerNudge1_UnHit()
    NudgeOn=1
  End Sub
 Sub TriggerNudge2_Hit()
    NudgeOn=0
  End Sub
 Sub TriggerNudge2_UnHit()
    NudgeOn=1
  End Sub
'-----------------------------------------------------------
'     SLINGS and WALLS
'-----------------------------------------------------------
Sub R14_Hit()
    If Tilt>2 Then Exit Sub
		AddScoresToPlayer 10,ActivePlayer
    AltLights
End Sub
Sub R30_Hit()
    If Tilt>2 Then Exit Sub
		AddScoresToPlayer 10,ActivePlayer
    AltLights
End Sub
Sub R1_Hit()
    If Tilt>2 Then Exit Sub
		AddScoresToPlayer 10,ActivePlayer
End Sub
'-------------------------------------------------------------------
'      Alternators
'-------------------------------------------------------------------

Sub AltLights()
   mem(1)=Abs(mem(1)-1)
   Select Case mem(1)
      Case 0
         SetBumperOn 1
         SetBumperOn 2
         SetBumperOff 3
         For each robject in BRolls 
		 robject.state = 0 	
		 Next
         mem(10)=1
         mem(11)=0
         FLSetOff 21,21
         FLSetOn 22,22
         FLSetOff 23,23
         
         If mem(16)>0  And mem(17)=0 AND sadjust = 1  AND ladjust = 1 Then
            FLSetOff 15,15 'Extra ball
         End If
         If mem(16)>1 And mem(18)=0 AND sadjust = 1  AND ladjust = 1 Then
            FLSetOn 14,14 'Special
         End If

         If mem(16)>0  And mem(17)=0 AND sadjust = 1 AND ladjust = 0 Then
            'FLSetOff 15,15 'Extra ball
         End If
         If mem(16)>1 And mem(18)=0 AND sadjust = 1 AND ladjust = 0 Then
            'FLSetOn 14,14 'Special
         End If



		 If mem(16)>0  And mem(17)=0 AND sadjust = 0  AND ladjust = 1 Then
            FLSetOff 15,15 'Extra ball
         End If
         If mem(16)>0 And mem(18)=0 AND sadjust = 0  AND ladjust = 1 Then
            FLSetOn 14,14 'Special
         End If

         If mem(16)>0  And mem(17)=0 AND sadjust = 0 AND ladjust = 0 Then
            'FLSetOff 15,15 'Extra ball
         End If
         If mem(16)>0 And mem(18)=0 AND sadjust = 0 AND ladjust = 0 Then
            'FLSetOn 14,14 'Special
         End If



         
      Case 1
         SetBumperOff 1
         SetBumperOff 2
         SetBumperOn 3
         For each robject in BRolls 
		 robject.state = 1 	
		 Next
         mem(10)=0
         mem(11)=1
         FLSetOn 21,21
         FLSetOff 22,22
         FLSetOn 23,23
         
         If mem(16)>0 And mem(17)=0 AND sadjust = 1 AND ladjust = 1 Then
            FLSetOn 15,15 'Extra ball
         End If
         If mem(16)>1 And mem(18)=0 AND sadjust = 1 AND ladjust = 1 Then
            FLSetOff 14,14 'Special
         End If
         
         If mem(16)>0 And mem(17)=0 AND sadjust = 1 AND ladjust = 0 Then
            'FLSetOn 15,15 'Extra ball
         End If
         If mem(16)>1 And mem(18)=0 AND sadjust = 1 AND ladjust = 0 Then
            'FLSetOff 14,14 'Special
         End If



         If mem(16)>0 And mem(17)=0 AND sadjust = 0 AND ladjust = 1 Then
            FLSetOn 15,15 'Extra ball
         End If
         If mem(16)>0 And mem(18)=0 AND sadjust = 0 AND ladjust = 1 Then
            FLSetOff 14,14 'Special
         End If
         
         If mem(16)>0 And mem(17)=0 AND sadjust = 0 AND ladjust = 0 Then
            'FLSetOn 15,15 'Extra ball
         End If
         If mem(16)>0 And mem(18)=0 AND sadjust = 0 AND ladjust = 0 Then
            'FLSetOff 14,14 'Special
         End If
         
   End Select
End Sub

Sub DTargs_Timer()
	me.enabled = 0
	If sw1p.TransZ < -20 and sw2p.TransZ < -20 and sw3p.TransZ < -20 and sw4p.TransZ < -20 and sw5p.TransZ < -20 Then
		AddScoresToPlayer 3000,ActivePlayer
		Resetdrops true
      
		If mem(16)<3 Then mem(16)=mem(16)+1

			Select case mem(16)
				Case 1 ' X-Ball light on
					If sadjust = 1 Then
						FLSetOn 15,15
					Else 
						FLSetOn 15,15
						FLSetOn 14,14
						FLSetOn 18,18
					End If
             
				Case 2 ' Special light on
					If sadjust = 1 Then
						FLSetOn 18,18
						FLSetOn 14,14 ' Lane Special
					Else
						ReplayAction
					End If
				Case 3 ' Reward Special
					ReplayAction
			End Select
		End If
End Sub

 '------------------------------------------------------------------------
 '             KICKERS
 '------------------------------------------------------------------------


 '------------------------------------------------------------------------
 '             BONUS
 '------------------------------------------------------------------------
Sub AddBonus(ByVal ab)
   If Tilt>2 Then Exit Sub
   mem(8)=mem(8)+ab
End Sub

Sub AddBonusTimer_Timer()
   If Tilt>2 Then mem(8)=0
   If mem(8)>0 Then
      If mem(9)<15 Then
         mem(9)=mem(9)+1
         PlaySound "SSResetBonusNoScore"
         SetBonusLights
      End If
      mem(8)=mem(8)-1
   End If   
End Sub

Dim BCC
Sub CollectBonusTimer_Timer()
   If mem(9)>0 Then
      If mem(12)=1 then
      BCC=BCC+1
      Select Case BCC
         Case 1
			AddScoresToPlayer 1000,ActivePlayer
            SReels(ActivePlayer).addvalue(1000)
            Exit Sub
         Case 2
            Exit Sub
         Case 3
            BCC=0
      End Select
   End If
   AddScoresToPlayer 1000,ActivePlayer
   SReels(ActivePlayer).addvalue(1000)
   PlaySound "SSResetBonusNoScore"
   mem(9)=mem(9)-1
   SetBonusLights
   Else
      CollectBonusTimer.Enabled = False
      SeqTimer.Interval=250
      SeqTimer.Enabled=True
      GameSeq=2
   End If
End Sub

Sub SetBonusLights()
   FLSetOff 1,10
   If mem(9)>0 Then
      If mem(9)<11 Then
         FLSetOn mem(9),mem(9)
      Else
         FLSetOn 10,10
         FLSetOn Mem(9)-10,mem(9)-10
      End If
   End If
End Sub

'------------------------------------------------------------------------
'             TARGETS, GATE, POST AND BUG
'------------------------------------------------------------------------

Sub UpPost()
   If Post.IsDropped=True Then
      Post.IsDropped=false
      PlaySound "solon"
   End If
End Sub

Sub DownPost()
   If Post.IsDropped=false Then
      Post.IsDropped=true   
      PlaySound "solon"
   End If
End Sub

Sub OpenGate()
   If mem(6)=0 Then
      Flipper1.RotateToEnd
	  Flipper1M.RotateToEnd
      PlaySoundat "OpenGate", Flipper1
      mem(6)=1
      FLSetOn 16,16
      DOF 150,2
   End If
End Sub

Sub CloseGate()
   If mem(6)=1 Then
      Flipper1.RotateToStart
	  Flipper1M.RotateToStart
      PlaySoundat "OpenGate", Flipper1
      mem(6)=0
      FLSetOff 16,16
      DOF 150,2
   End If
End Sub

Sub OpenTopGate()
   If mem(5)=0 Then
      Gate2.Open = True
      PlaySound "OpenGate"
      mem(5)=1
      FLSetOn 32,32
    End If
End Sub

Sub CloseTopGate()
   If mem(5)=1 Then
      Gate2.Open = False
      PlaySound "OpenGate"
      mem(5)=0
      FLSetOff 32,32
   End if
End Sub
 

 '------------------------------------------------------------------------
 '             SOUNDS
 '------------------------------------------------------------------------
 


Dim RStep, Lstep, Tstep, Blstep, TLstep, BRstep, TRstep
Dim dtest
Sub LeftSlingShot_Slingshot
	 If Tilt>2 Then Exit Sub
    RandomSoundSlingshotLeft sling3
    LSling.Visible = 0
    LSling1.Visible = 1
    sling3.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval  = 10
    AddScoresToPlayer 10,ActivePlayer
	DOF 106, DOFPulse
    AltLights
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling3.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling3.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	 If Tilt>2 Then Exit Sub
    RandomSoundSlingshotRight sling4
    RSling.Visible = 0
    RSling1.Visible = 1
    SLING4.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval  = 10
    AddScoresToPlayer 10,ActivePlayer
	DOF 107, DOFPulse
    AltLights
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling4.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling4.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub TopSlingShot_Hit
	 If Tilt>2 Then Exit Sub
    TSling.Visible = 0
    TSling1.Visible = 1
    TStep = 0
    TopSlingShot.TimerEnabled = 1
	TopSlingShot.TimerInterval  = 10
    AddScoresToPlayer 10,ActivePlayer
    AltLights
End Sub

Sub TopSlingShot_Timer
    Select Case TStep
        Case 3:TSLing1.Visible = 0:TSLing2.Visible = 1
        Case 4:TSLing2.Visible = 0:TSLing.Visible = 1:TopSlingShot.TimerEnabled = 0
    End Select
    TStep = TStep + 1
End Sub


Sub Leftflipper_Collide(parm)
    CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub Rightflipper_Collide(parm)	
    CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    RightFlipperCollide parm  'This is the Fleep code
End Sub

Sub Leftflipper2_Collide(parm)
 	CheckLiveCatch Activeball, LeftFlipper2, LF2Count, parm
    LeftFlipperCollide parm  'This is the Fleep code
End Sub

Sub flipper5_Collide(parm)
 	RightFlipperCollide parm
End Sub

 
'---------------------------------------------------------------------
'       Handle Replay and Hiscore
'---------------------------------------------------------------------

Sub ReplayAction()
	If Coins<9 Then Coins=Coins+1
	If VRRoom = 1 Then
		cred =reels(4, 0)
		reels(4, 0) = 0
		SetDrum -1,0,  0

		SetReel 0,-1,  Coins
		reels(4, 0) = Coins
	End If
	If B2SOn Then Controller.B2SSetCredits Coins
	KnockerSolenoid
	DOF 108, DOFPulse
	DOF 132, DOFPulse	
End Sub

Function GetScore(ByVal pl)
   Dim temphi
   pl=(pl-1)*6
   temphi=ReelPulses(pl+6,2)
   temphi=temphi+(ReelPulses(pl+5,2)*10)
   temphi=temphi+(ReelPulses(pl+4,2)*100)
   temphi=temphi+(ReelPulses(pl+3,2)*1000)
   temphi=temphi+(ReelPulses(pl+2,2)*10000)
   temphi=temphi+(ReelPulses(pl+1,2)*100000)
   GetScore=temphi
End Function

 '----------------------------------------------------
 '   RULES
 '----------------------------------------------------
 
 sub DisplayRuleSheet()
	dim rules

	msgBox rules, vbOKonly, "       RULESHEET --- WILLIAMS/DOODLE BUG"
End Sub



'********************************************************************************************
Sub UpdateBIP()
	If BallsToPlay=3 Then
		If B2SOn Then
		Controller.B2SSetBallinPlay 0
		Controller.B2SSetBallinPlay 4-Round
        End If
        If Round = 3 Then
        B1BG.state = 1
        Else
        B1BG.state = 0
        End If
        If Round = 2 Then
        B2BG.state = 1
        Else
        B2BG.state = 0
        End If
        If Round = 1 Then
        B3BG.state = 1
        Else
        B3BG.state = 0
        End If
		'End If
	Else
		If B2SOn Then
		Controller.B2SSetBallinPlay 0
		Controller.B2SSetBallinPlay 6-Round
        End If
        If Round = 5 Then
        B1BG.state = 1
        Else
        B1BG.state = 0
        End If
        If Round = 4 Then
        B2BG.state = 1
        Else
        B2BG.state = 0
        End If
        If Round = 3 Then
        B3BG.state = 1
        Else
        B3BG.state = 0
        End If
        If Round = 2 Then
        B4BG.state = 1
        Else
        B4BG.state = 0
        End If
        If Round = 1 Then
        B5BG.state = 1
        Else
        B5BG.state = 0
        'End If
		End If
	End If
	If VRRoom = 1 Then
		FlasherBalls
	End If
End Sub

Sub ResetMemory(ByVal All)
    If All=1 Then
       For i=0 to 30
          Mem(i)=0
       Next
    Else
       For i=5 to 30
          Mem(i)=0
       Next
    End If
End Sub

Sub PRreset()
   For  i=1 To 10
      PLU(i).WidthBottom = 0:PLU(i).WidthTop = 0
   Next
   PLU(i).WidthBottom = 40:PLU(i).WidthTop = 30
   PRLight.State=Abs(PRLight.State-1)
End Sub

Sub PRLight_Timer()
   If PRdir=0 Then
      If PRpull<10 Then
         PLU(PRpull).WidthBottom = 0:PLU(PRpull).WidthTop = 0
         PRpull=PRpull+1
         PLU(PRpull).WidthBottom = 30:PLU(PRpull).WidthTop = 30
         PRLight.State=Abs(PRLight.State-1)
      End If
   Else
      If PRpull>0 Then
         PLU(PRpull).WidthBottom = 0:PLU(PRpull).WidthTop = 0
         PRpull=PRpull-3
         If PRpull<0 Then PRpull=0
         PLU(PRpull).WidthBottom = 30:PLU(PRpull).WidthTop = 30
         PRLight.State=Abs(PRLight.State-1)
      Else
         PRLight.TimerEnabled=false
      End If
   End If
End Sub


'*********************************************************************************************
'*****************               FADING LIGHTS CODE                ***************************
'*****************                     ROSVE                       ***************************
'*********************************************************************************************
Dim FLData(100,7)
Dim FLMaxLight
FLMaxLight=23
' 1=command --> 0=set off   1=set on   2=flash   99=no change
' 2=state
' 3=repeats   999=continous
' 4=on duration
' 5=off duration
' 6=on dur mem
' 7=off dur mem

Sub FLSetOn(ByVal FLlight1, ByVal FLlight2)
   For i=FLlight1 to  FLlight2
      FLData(i,1)=1
      FLData(i,2)=0
      FLData(i,3)=0
      FLData(i,4)=0
      FLData(i,5)=0
   Next
End Sub

Sub FLSetOff(ByVal FLlight1, ByVal FLlight2)
   For i=FLlight1 to  FLlight2
      FLData(i,1)=0
      FLData(i,2)=4
      FLData(i,3)=0
      FLData(i,4)=0
      FLData(i,5)=0
   Next
End Sub

Sub FLFlash(ByVal FLlight1, ByVal FLlight2, ByVal FLrepeat, ByVal FLondur, ByVal FLoffdur)
   For i=FLlight1 to  FLlight2
      FLData(i,1)=2
      FLData(i,2)=0
      FLData(i,3)=FLrepeat
      FLData(i,4)=FLondur
      FLData(i,5)=FLoffdur
      FLData(i,6)=FLondur
      FLData(i,7)=FLoffdur
   Next
End Sub

Sub FLTimer_Timer()
   For i=1 To FLMaxLight
      Select Case FLData(i,1)
      Case 0
         FLData(i,2)=FLData(i,2)+1
         Select Case FLData(i,2)
         Case 5
            'L2(i).State=0
         Case 7
            L1(i).State=0
            FLData(i,1)=99
         End Select
      Case 1
         FLData(i,2)=FLData(i,2)+1
         Select Case FLData(i,2)
         Case 1
            L1(i).State=1
         Case 3
            'L2(i).State=1
            FLData(i,1)=99
         End Select

      Case 2
         FLData(i,2)=FLData(i,2)+1
         Select Case FLData(i,2)
         Case 1
            L1(i).State=1
         Case 3
            'L2(i).State=1
         Case 4 'on Duration
            IF FLData(i,4)>0 Then
               FLData(i,4)=FLData(i,4)-1
               FLData(i,2)=FLData(i,2)-1
            Else
               FLData(i,4)=FLData(i,6)
            End If
         Case 5
            'L2(i).State=0
         Case 7
            L1(i).State=0
            IF FLData(i,3)>0 Then
               IF FLData(i,5)>0 Then
                  FLData(i,5)=FLData(i,5)-1
                  FLData(i,2)=FLData(i,2)-1
                Else
                      FLData(i,5)=FLData(i,7)
                      FLData(i,3)=FLData(i,3)-1
                      If FLData(i,3)=998 Then FLData(i,3)=999
                      FLData(i,2)=0
                End If
            Else
               FLData(i,1)=99
            End If
         End Select
      End Select
   Next
End Sub

'********************
'Dim d(20),b(20)
'
Sub Update_Stuff_Timer()
	DiverterP.ObjRotZ = Flipper1.CurrentAngle + 90
	DiverterPM.RotY = Flipper1M.CurrentAngle
	
	If sw1p.TransZ < -20 Then
		DT5L.state = 1
		DT5P.visible = 0
	Else
		DT5L.state = 0
		DT5P.visible = 1
	End If
	
	If sw2p.TransZ < -20 Then
		DT4L.state = 1
		DT4P.visible = 0
	Else
		DT4L.state = 0
		DT4P.visible = 1
	End If
	
	If sw3p.TransZ < -20 Then
		DT3L.state = 1
		DT3P.visible = 0
	Else
		DT3L.state = 0
		DT3P.visible = 1
	End If
	
	If sw4p.TransZ < -20 Then
		DT2L.state = 1
		DT2P.visible = 0
	Else
		DT2L.state = 0
		DT2P.visible = 1
	End If
	
	If sw5p.TransZ < -20 Then
		DT1L.state = 1
		DT1P.visible = 0
	Else
		DT1L.state = 0
		DT1P.visible = 1
	End If

	ScoreReel12.setvalue(HiScore)

	BallReflectionUpdate

	If ActivePlayer = 1 Then
		P1BG.State = 1
	Else
		P1BG.State = 0
	End If

	If ActivePlayer = 2 Then
		P2BG.State = 1
	Else
		P2BG.State = 0
	End If

	If ActivePlayer = 3 Then
		P3BG.State = 1
	Else
		P3BG.State = 0
	End If

	If ActivePlayer = 4 Then
		P4BG.State = 1
	Else
		P4BG.State = 0
	End If

	ScoreReel5.setvalue(Coins)


	If MaxPlayers = 1 Then
		MP1BG.state = 1
	Else
		MP1BG.State = 0
	End If

	If MaxPlayers = 2 Then
		MP2BG.state = 1
	Else
		MP2BG.State = 0
	End If

	If MaxPlayers = 3 Then
		MP3BG.state = 1
	Else
		MP3BG.State = 0
	End If

	If MaxPlayers = 4 Then
		MP4BG.state = 1
	Else
		MP4BG.State = 0
	End If

	If Tilt > 2.9 Then
		TILTBG.state = 1
	Else
		TILTBG.state = 0
	End If

	If b3Bumper.State = 1 Then
		B3S.visible = 0
	Else
		B3S.visible = 1
	End If

	If b2Bumper.State = 1 Then
		B2S.visible = 0
	Else
		B2S.visible = 1
	End If

	If b1Bumper.State = 1 Then
		B1S.visible = 0
	Else
		B1S.visible = 1
	End If

	DT5P7.X = Gate1.CurrentAngle + 152
	DT5P8.X = Gate2.CurrentAngle + 733

'Handle Cabinet View HighScore.

'Split HiScore intvar into string and 
'assign to separate reels.

	Dim str:str = CStr(right("000000" & HiScore,6))
	Dim I

	If Coins > 0 Then
		Clight.State = 1
	Else
		Clight.State = 0
	End If

	If VRRoom > 0 Then
		Bumper1BM.visible = b1Bumper.state
		Bumper1M.visible = b1Bumper.state
		Bumper2BM.visible = b2Bumper.state
		Bumper2M.visible = b2Bumper.state
		Bumper3BM.visible = b3Bumper.state
		Bumper3M.visible = b3Bumper.state
		Light1_X1M.visible = Light1_X1.state
		Light1_X2M.visible = Light1_X2.state
		Light1_X3M.visible = Light1_X3.state
		Light1_X4M.visible = Light1_X4.state
		Light1_X5M.visible = Light1_X5.state
		Light1_X6M.visible = Light1_X6.state
		Light1_X7M.visible = Light1_X7.state
		Light1_X8M.visible = Light1_X8.state
		Light1_X9M.visible = Light1_X9.state
		Light1_X10M.visible = Light1_X10.state
		Light1_X11M.visible = Light1_X11.state
		Light1_X12M.visible = Light1_X12.state
		Light1_X13M.visible = Light1_X13.state
		Light1_X14M.visible = Light1_X14.state
		Light1_X15M.visible = Light1_X15.state
		Light1_X16M.visible = Light1_X16.state
		Light1_X17M.visible = Light1_X17.state
		Light1_X18M.visible = Light1_X18.state
		Light1_X21M.visible = Light1_X21.state
		Light1_X22M.visible = Light1_X22.state
		Light1_X23M.visible = Light1_X23.state
		LDT1M.visible = LDT1.state
		LDT2M.visible = LDT2.state
		LDT3M.visible = LDT3.state
		LDT4M.visible = LDT4.state
		LDT5M.visible = LDT5.state
		LDT6M.visible = LDT6.state
		LDT15M.visible = LDT15.state
		LDT16M.visible = LDT16.state
		sw1pM.Transz = sw1p.Transz
		sw2pM.Transz = sw2p.Transz
		sw3pM.Transz = sw3p.Transz
		sw4pM.Transz = sw4p.Transz
		sw5pM.Transz = sw5p.Transz
	End If	
End Sub

Dim anidir, aniup, anidown

Sub IC_ani_Timer
Select Case anidir
Case 1:
Card3.visible = 1:Card4.visible = 1:Light1.State = 1:Light2.State = 1
If Card3.X => 486 Then
aniup = 1:anidown = 0
me.enabled = 0
bulb_move.enabled = 1
End If
Card3.size_X = Card3.size_X + 0.012
Card3.size_Y = Card3.size_Y + 0.012
Card3.Y = Card3.Y - 4
Card3.X = Card3.X + 2

Card4.size_X = Card4.size_X + 0.012
Card4.size_Y = Card4.size_Y + 0.012
Card4.Y = Card4.Y - 4
Card4.X = Card4.X + 2

Case 2:
If Card3.X <= 156.4871 Then
Card3.visible = 0:Card4.visible = 0
aniup = 0:anidown = 1
Light1.X = 115
Light2.X = 865
me.enabled = 0
End If
Card3.size_X = Card3.size_X - 0.012
Card3.size_Y = Card3.size_Y - 0.012
Card3.Y = Card3.Y + 4
Card3.X = Card3.X - 2

Card4.size_X = Card4.size_X - 0.012
Card4.size_Y = Card4.size_Y - 0.012
Card4.Y = Card4.Y + 4
Card4.X = Card4.X - 2
End Select
End Sub

Sub Bulb_Move_Timer
If Light1.X => 840 Then
Light1.State = 0:light2.State = 0
me.enabled = 0
End If
Light1.X = Light1.X + 3
Light2.X = Light2.X - 3
End Sub

Dim plasdir

plasdir = 1

Sub plas_timer()
	Select Case plasdir
		Case 1:Glass.image = "plasticsr1":plasdir = 2
		Case 2:Glass.image = "plasticsr2":plasdir = 3
		Case 3:Glass.image = "plasticsr3":plasdir = 4
		Case 4:Glass.image = "plasticsr2":plasdir = 5
		Case 5:Glass.image = "plasticsr1":plasdir = 6
		Case 6:Glass.image = "plastics2":me.enabled = 0
		Case 7:Glass.image = "plastics_sr1":plasdir = 8
		Case 8:Glass.image = "plastics_sr2":plasdir = 9
		Case 9:Glass.image = "plastics_sr3":plasdir = 10
		Case 10:Glass.image = "plastics_sr2":plasdir = 11
		Case 11:Glass.image = "plastics_sr1":plasdir = 12
		Case 12:Glass.image = "plastics2":me.enabled = 0:
	End Select
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

Sub FrameTimer_Timer()
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	Lflipmesh1.RotZ = LeftFlipper.CurrentAngle
	Rflipmesh1.RotZ = RightFlipper.CurrentAngle
	Lflipmesh2.RotZ = LeftFlipper2.CurrentAngle
	Rflipmesh2.Rotz = Flipper5.CurrentAngle
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperLSh1.RotZ = LeftFlipper2.CurrentAngle
	FlipperRSh1.RotZ = Flipper5.CurrentAngle
	Lflipmesh001.RotZ = LeftFlipperMirror1.CurrentAngle
	Lflipmesh002.RotZ = LeftFlipperMirror2.CurrentAngle
	Rflipmesh001.RotZ = RightFlipperMirror.CurrentAngle
	Rflipmesh002.RotZ = RightFlipperMirror2.CurrentAngle
End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
Dim tablewidth: tablewidth = captfant.width
Dim tableheight: tableheight = captfant.height

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
'	...For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	...Next
'
' ...rolling and drop sounds...

'		...If DropCount(b) < 5 Then
'			...DropCount(b) = DropCount(b) + 1
'		...End If
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

Dim PI: PI = 4*Atn(1)

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
					objBallShadow(s).X = gBot(s).X + offsetX - (tablewidth/2 - BOT(s).X)/(60/AmbientMovement)
'					If gBOT(s).X < tablewidth/2 Then
'						objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
'					Else
'						objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
'					End If
					objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				End If

			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s
				objBallShadow(s).X = gBOT(s).X + offsetX - (tablewidth/2 - gBOT(s).X)/(60/AmbientMovement)
'				If gBOT(s).X < tablewidth/2 Then
'					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
'				Else
'					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
'				End If
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
				BallShadowA(s).X = gBOT(s).X + offsetX - (tablewidth/2 - gBOT(s).X)/(60/AmbientMovement)
'				If gBOT(s).X < tablewidth/2 Then
'					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
'				Else
'					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
'				End If
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
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

'*****************************************
'	Ball Reflection
'*****************************************

Sub BallReflectionUpdate()
    Dim BOT, b
    BOT = GetBalls

'	' render the shadow for each ball
'    For b = 0 to UBound(BOT)
'		BallShadow(b).X = Bot(b).X - (captfant.Width/2 - BOT(b).X)/30 
'		BallShadow(b).Y = BOT(b).Y
'		BallShadow(b).Z = 1
'		If BOT(b).Z > 20 Then
'			BallShadow(b).visible = 1
'		Else
'			BallShadow(b).visible = 0
'		End If
'	Next
		If VRRoom = 1 Then
			If Ballinplay = 1 Then
				pBallRefl.visible = 1
				pBallRefl.x = ball.x
				pBallRefl.y = -ball.y
				If Ball.y < 251 Then
					pBallRefl.z = ball.z - 0.13 * ball.y
				ElseIf Ball.y > 250 and Ball.y < 451 Then
					pBallRefl.z = ball.z - 0.155 * ball.y
				ElseIf Ball.y > 450 and Ball.y < 901 Then
					pBallRefl.z = ball.z -0.18 * ball.y
				ElseIf Ball.y > 900 and Ball.y < 1351 Then
					pBallRefl.z = ball.z -0.19 * ball.y
				ElseIf Ball.y > 1350 Then
					pBallRefl.z = ball.z - 0.2 * ball.y
				End If
			Else
				pBallRefl.visible = 0
			End If
		End If
	
End Sub

Sub captfant_Exit
	If B2SOn Then Controller.stop
End Sub


'////////////////////////////////////////////////////////////////



'******************* VR Plunger **********************

Sub TimerVRPlunger_Timer
  If VR_Primary_plunger.Y < 2203 then
       VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	VR_Primary_plunger.Y = 2083 + (5* Plunger.Position) -20
End Sub



' ***************************************************************************
'          Reel Code(EM) 1-4 player 5x drums, 1 credit drum CORE CODE
' ****************************************************************************


' ********************* POSITION EM REEL DRUMS ON BACKGLASS *************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen
Dim inx
xoff =480
yoff = 0
zoff =835
xrot = -90

Const USEEMS = 4 ' 1-4 set between 1 to 4 based on number of players

const idx_emp1r1 =0 'player 1
const idx_emp2r1 =5 'player 2
const idx_emp3r1 =10 'player 3
const idx_emp4r1 =15 'player 4
const idx_emp4r6 =20 'credits


Dim BGObjEM(1)
if USEEMS = 1 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 2 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 3 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	emp3r1, emp3r2, emp3r3, emp3r4, emp3r5, _
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 4 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	emp3r1, emp3r2, emp3r3, emp3r4, emp3r5, _
	emp4r1, emp4r2, emp4r3, emp4r4, emp4r5, _
	emp4r6) ' credits
end If

Sub center_objects_em()
Dim cnt,ii, xx, yy, yfact, xfact, objs
'exit sub
yoff = -150
zscale = 0.0000001
xcen =(960 /2) - (17 / 2)
ycen = (1065 /2 ) + (313 /2)

yfact = -35
xfact = -9

cnt =0
	For Each objs In BGObjEM(0)
	If Not IsEmpty(objs) then
		if objs.name = emp4r6.name then
		yoff = -22 ' credit drum is 60% smaller
		Else
		yoff = -76
		end if

	xx =objs.x 
		
	objs.x = (xoff - xcen) + xx + xfact
	yy = objs.y 
	objs.y =yoff 

		If yy < 0 then
		yy = yy * -1
		end if

	objs.z = (zoff - ycen) + yy - (yy * zscale) + yfact
	
	'objs.rotx = xrot
	end if
	cnt = cnt + 1
	Next

end sub


' ********************* UPDATE EM REEL DRUMS CORE LIB *************************

Dim cred,ix, np,npp, reels(5, 7), scores(6,2)

'reset scores to defaults
for np =0 to 5 
scores(np,0 ) = 0
scores(np,1 ) = 0
Next

'reset EM drums to defaults
For np =0 to 3 
	For  npp =0 to 6
	reels(np, npp) =0 ' default to zero 
	Next
Next


Sub SetScore(player, ndx , val) 

Dim ncnt 

	if player = 5 or player = 6 then
		if val > 0 then
			If(ndx = 0)Then ncnt = val * 10
			If(ndx = 1)Then ncnt = val 

			scores(player, 0) = scores(player, 0) + ncnt  
		end if
	else
		if val > 0 then

		If(ndx = 0)then ncnt = val * 10000
		If(ndx = 1)then ncnt = val * 1000
		If(ndx = 2)Then ncnt = val * 100
		If(ndx = 3)Then ncnt = val * 10
		If(ndx = 4)Then ncnt = val 

		scores(player, 0) = scores(player, 0) + ncnt  
		'scores(player, 0) + ncnt  

		end if
	end if
End Sub


Sub SetDrum(player, drum , val) 
Dim cnt
Dim objs : objs =BGObjEM(0)

	If val = 0 then
		Select case player
		case -1: ' the credit drum
		If Not IsEmpty(objs(idx_emp4r6)) then
		objs(idx_emp4r6).ObjrotX = 0 ' 285
		'cnt =objs(idx_emp4r6).ObjrotX
		end if
		Case 0: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp1r1)) then: objs(idx_emp1r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp1r1+1)) then: objs(idx_emp1r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp1r1+2)) then: objs(idx_emp1r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp1r1+3)) then: objs(idx_emp1r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp1r1+4)) then: objs(idx_emp1r1+4).ObjrotX=0: end if
		End Select
		Case 1: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp2r1)) then: objs(idx_emp2r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp2r1+1)) then: objs(idx_emp2r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp2r1+2)) then: objs(idx_emp2r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp2r1+3)) then: objs(idx_emp2r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp2r1+4)) then: objs(idx_emp2r1+4).ObjrotX=0: end if
		End Select
		Case 2: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp3r1)) then: objs(idx_emp3r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp3r1+1)) then: objs(idx_emp3r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp3r1+2)) then: objs(idx_emp3r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp3r1+3)) then: objs(idx_emp3r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp3r1+4)) then: objs(idx_emp3r1+4).ObjrotX=0: end if
		End Select
		Case 3: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp4r1)) then: objs(idx_emp4r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp4r1+1)) then: objs(idx_emp4r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp4r1+2)) then: objs(idx_emp4r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp4r1+3)) then: objs(idx_emp4r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp4r1+4)) then: objs(idx_emp4r1+4).ObjrotX=0: end if
		End Select
	End Select

	else
	Select case player

		Case -1: ' the credit drum
		'emp4r6.ObjrotX = emp4r6.ObjrotX + val
		If Not IsEmpty(objs(idx_emp4r6)) then
		objs(idx_emp4r6).ObjrotX = objs(idx_emp4r6).ObjrotX + val 
		end if

		Case 0: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp1r1)) then: objs(idx_emp1r1).ObjrotX= objs(idx_emp1r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp1r1+1)) then: objs(idx_emp1r1+1).ObjrotX= objs(idx_emp1r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp1r1+2)) then: objs(idx_emp1r1+2).ObjrotX= objs(idx_emp1r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp1r1+3)) then: objs(idx_emp1r1+3).ObjrotX= objs(idx_emp1r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp1r1+4)) then: objs(idx_emp1r1+4).ObjrotX= objs(idx_emp1r1+4).ObjrotX + val: end if
		End Select
		Case 1: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp2r1)) then: objs(idx_emp2r1).ObjrotX= objs(idx_emp2r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp2r1+1)) then: objs(idx_emp2r1+1).ObjrotX= objs(idx_emp2r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp2r1+2)) then: objs(idx_emp2r1+2).ObjrotX= objs(idx_emp2r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp2r1+3)) then: objs(idx_emp2r1+3).ObjrotX= objs(idx_emp2r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp2r1+4)) then: objs(idx_emp2r1+4).ObjrotX= objs(idx_emp2r1+4).ObjrotX + val: end if
		End Select
		Case 2: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp3r1)) then: objs(idx_emp3r1).ObjrotX= objs(idx_emp3r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp3r1+1)) then: objs(idx_emp3r1+1).ObjrotX= objs(idx_emp3r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp3r1+2)) then: objs(idx_emp3r1+2).ObjrotX= objs(idx_emp3r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp3r1+3)) then: objs(idx_emp3r1+3).ObjrotX= objs(idx_emp3r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp3r1+4)) then: objs(idx_emp3r1+4).ObjrotX= objs(idx_emp3r1+4).ObjrotX + val: end if
		End Select
		Case 3: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp4r1)) then: objs(idx_emp4r1).ObjrotX= objs(idx_emp4r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp4r1+1)) then: objs(idx_emp4r1+1).ObjrotX= objs(idx_emp4r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp4r1+2)) then: objs(idx_emp4r1+2).ObjrotX= objs(idx_emp4r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp4r1+3)) then: objs(idx_emp4r1+3).ObjrotX= objs(idx_emp4r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp4r1+4)) then: objs(idx_emp4r1+4).ObjrotX= objs(idx_emp4r1+4).ObjrotX + val: end if
		End Select

	End Select
	end if
End Sub


Sub SetReel(player, drum, val)

'TextBox1.text = "playr:" & player +1 & " drum:" & drum & "val:" & val  

Dim  inc , cur, dif, fix, fval

inc = 33.5
fval = -5 ' graphic seam between 5 & 6 fix value, easier to fix here than photoshop 

If  (player <= 3) or (drum = -1) then

	If drum = -1 then drum = 0

	cur =reels(player, drum)	

	If val <> cur then ' something has changed
	Select Case drum

		Case 0: ' credits drum

			if val > cur then
				dif =val - cur
				fix =0
					If cur < 5 and cur+dif > 5 then
					fix = fix- fval
					end if
				dif = dif * inc
					
				dif = dif-fix
					
				SetDrum -1,0,  -dif
			Else
				if val = 0 Then	
				SetDrum -1,0,  0' reset the drum to abs. zero
				Else
				dif = 11 - cur
				dif = dif + val

				dif = dif * inc
				dif = dif-fval

				SetDrum -1,0,   -dif
				end if
			end if
		Case 1: 
		'TB1.text = val
		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
				
			dif = dif-fix
				
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val

			dif = dif * inc
			dif = dif-fval

			SetDrum player,drum,   -dif
			end if

		end if
		reels(player, drum) = val

		Case 2: 
		'TB2.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if
		end if
		reels(player, drum) = val

		Case 3: 
		'TB3.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val

		Case 4: 
		'TB4.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif 
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val

		Case 5: 
		'TB5.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif 
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val
	 End Select

	end if
end if
End Sub

Dim EMMODE: EMMODE = 0
'Dim Score10,Score100,Score1000,Score10000,Score100000
'Dim Score1000,Score10000,Score100000, ActivePLayer
Dim nplayer,playr,value,curscr,curplayr

' EMMODE = 1
'ex: UpdateReels 0-3, 0-3, 0-199999, n/a, n/a, n/a, n/a, n/a, n/a
' EMMODE = 0
'ex: UpdateReels 0-3, 0-3, n/a, 0-1,0-99999 ,0-9999, 0-999, 0-99, 0-9  

Sub UpdateReels (Player,nReels ,nScore, n100K, Score10000 ,Score1000,Score100,Score10,Score1)

'if USEEM Then

' to-do find out if player is one or zero based, if 1 based subtract 1.
value =nScore'Score(Player)
	nplayer = Player -1

	curscr = value
	curplayr = nplayer


scores(0,1) = scores(0,0)
	scores(0,0) = 0
	scores(1,1) = scores(1,0)
	scores(1,0) = 0
	scores(2,1) = scores(2,0)
	scores(2,0) = 0
	scores(3,1) = scores(3,0)
	scores(3,0) = 0

	For  ix =0 to 6
		reels(0, ix) =0 
		reels(1, ix) =0 
		reels(2, ix) =0 
		reels(3, ix) =0 
	Next
	
	For  ix =0 to 4
		
	SetDrum ix, 1 , 0 
	SetDrum ix, 2 , 0  
	SetDrum ix, 3 , 0  
	SetDrum ix, 4 , 0  
	SetDrum ix, 5 , 0  
		
	Next 
	
	For playr =0 to nReels
	
		if EMMODE = 0 then
		If (ActivePLayer) = playr Then
		nplayer = playr

		SetReel nplayer, 1 , Score10000 : SetScore nplayer,0,Score10000
		SetReel nplayer, 2 , Score1000 : SetScore nplayer,1,Score1000
		SetReel nplayer, 3 , Score100 : SetScore nplayer,2,Score100
		SetReel nplayer, 4 , Score10 : SetScore nplayer,3,Score10 
		SetReel nplayer, 5 , 0 : SetScore nplayer,4,0 ' assumes ones position is always zero 

		else
		nplayer = playr
		value =scores(nplayer, 1) 
	
	
	' do ten thousands
		if(value >= 90000)  then:  SetReel nplayer, 1 , 9 : SetScore nplayer,0,9 : value = value - 90000: end if
		if(value >= 80000)  then:  SetReel nplayer, 1 , 8 : SetScore nplayer,0,8 : value = value - 80000: end if
		if(value >= 70000)  then:  SetReel nplayer, 1 , 7 : SetScore nplayer,0,7 : value = value - 70000: end if
		if(value >= 60000)  then:  SetReel nplayer, 1 , 6 : SetScore nplayer,0,6 : value = value - 60000: end if
		if(value >= 50000)  then:  SetReel nplayer, 1 , 5 : SetScore nplayer,0,5 : value = value - 50000: end if
		if(value >= 40000)  then:  SetReel nplayer, 1 , 4 : SetScore nplayer,0,4 : value = value - 40000: end if
		if(value >= 30000)  then:  SetReel nplayer, 1 , 3 : SetScore nplayer,0,3 : value = value - 30000: end if
		if(value >= 20000)  then:  SetReel nplayer, 1 , 2 : SetScore nplayer,0,2 : value = value - 20000: end if
		if(value >= 10000)  then:  SetReel nplayer, 1 , 1 : SetScore nplayer,0,1 : value = value - 10000: end if


	' do thousands
		if(value >= 9000)  then:  SetReel nplayer, 2 , 9 : SetScore nplayer,1,9 : value = value - 9000: end if
		if(value >= 8000)  then:  SetReel nplayer, 2 , 8 : SetScore nplayer,1,8 : value = value - 8000: end if
		if(value >= 7000)  then:  SetReel nplayer, 2 , 7 : SetScore nplayer,1,7 : value = value - 7000: end if
		if(value >= 6000)  then:  SetReel nplayer, 2 , 6 : SetScore nplayer,1,6 : value = value - 6000: end if
		if(value >= 5000)  then:  SetReel nplayer, 2 , 5 : SetScore nplayer,1,5 : value = value - 5000: end if
		if(value >= 4000)  then:  SetReel nplayer, 2 , 4 : SetScore nplayer,1,4 : value = value - 4000: end if
		if(value >= 3000)  then:  SetReel nplayer, 2 , 3 : SetScore nplayer,1,3 : value = value - 3000: end if
		if(value >= 2000)  then:  SetReel nplayer, 2 , 2 : SetScore nplayer,1,2 : value = value - 2000: end if
		if(value >= 1000)  then:  SetReel nplayer, 2 , 1 : SetScore nplayer,1,1 : value = value - 1000: end if

		'do hundreds
		
		if(value >= 900) then: SetReel nplayer, 3 , 9 : SetScore nplayer,2,9 : value = value - 900: end if
		if(value >= 800) then: SetReel nplayer, 3 , 8 : SetScore nplayer,2,8 : value = value - 800: end if
		if(value >= 700) then: SetReel nplayer, 3 , 7 : SetScore nplayer,2,7 : value = value - 700: end if
		if(value >= 600) then: SetReel nplayer, 3 , 6 : SetScore nplayer,2,6 : value = value - 600: end if
		if(value >= 500) then: SetReel nplayer, 3 , 5 : SetScore nplayer,2,5 : value = value - 500: end if
		if(value >= 400) then: SetReel nplayer, 3 , 4 : SetScore nplayer,2,4 : value = value - 400: end if
		if(value >= 300) then: SetReel nplayer, 3 , 3 : SetScore nplayer,2,3 : value = value - 300: end if
		if(value >= 200) then: SetReel nplayer, 3 , 2 : SetScore nplayer,2,2 : value = value - 200: end if
		if(value >= 100) then: SetReel nplayer, 3 , 1 : SetScore nplayer,2,1 : value = value - 100: end if

		'do tens
		if(value >= 90) then: SetReel nplayer, 4 , 9 : SetScore nplayer,3,9 : value = value - 90: end if
		if(value >= 80) then: SetReel nplayer, 4 , 8 : SetScore nplayer,3,8 : value = value - 80: end if
		if(value >= 70) then: SetReel nplayer, 4 , 7 : SetScore nplayer,3,7 : value = value - 70: end if
		if(value >= 60) then: SetReel nplayer, 4 , 6 : SetScore nplayer,3,6 : value = value - 60: end if
		if(value >= 50) then: SetReel nplayer, 4 , 5 : SetScore nplayer,3,5 : value = value - 50: end if
		if(value >= 40) then: SetReel nplayer, 4 , 4 : SetScore nplayer,3,4 : value = value - 40: end if
		if(value >= 30) then: SetReel nplayer, 4 , 3 : SetScore nplayer,3,3 : value = value - 30: end if
		if(value >= 20) then: SetReel nplayer, 4 , 2 : SetScore nplayer,3,2 : value = value - 20: end if
		if(value >= 10) then: SetReel nplayer, 4 , 1 : SetScore nplayer,3,1 : value = value - 10: end if

		'do ones
		if(value >= 9) then: SetReel nplayer, 5 , 9 : SetScore nplayer,4,9 : value = value - 9: end if
		if(value >= 8) then: SetReel nplayer, 5 , 8 : SetScore nplayer,4,8 : value = value - 8: end if
		if(value >= 7) then: SetReel nplayer, 5 , 7 : SetScore nplayer,4,7 : value = value - 7: end if
		if(value >= 6) then: SetReel nplayer, 5 , 6 : SetScore nplayer,4,6 : value = value - 6: end if
		if(value >= 5) then: SetReel nplayer, 5 , 5 : SetScore nplayer,4,5 : value = value - 5: end if
		if(value >= 4) then: SetReel nplayer, 5 , 4 : SetScore nplayer,4,4 : value = value - 4: end if
		if(value >= 3) then: SetReel nplayer, 5 , 3 : SetScore nplayer,4,3 : value = value - 3: end if
		if(value >= 2) then: SetReel nplayer, 5 , 2 : SetScore nplayer,4,2 : value = value - 2: end if
		if(value >= 1) then: SetReel nplayer, 5 , 1 : SetScore nplayer,4,1 : value = value - 1: end if

		end if
		Else
			If curplayr = playr Then
			nplayer = curplayr
			value = curscr
			else
			value =scores(playr, 1) ' store score
			nplayer = playr
			end if

		scores(playr, 0)  = 0 ' reset score

		if(value >= 100000) then

			'if nplayer = 0 then: FL100K1.visible = 1
			'if nplayer = 1 then: FL100K2.visible = 1
			'if nplayer = 2 then: FL100K3.visible = 1
			'if nplayer = 3 then: FL100K4.visible = 1

		value = value - 100000
		
		end if


	' do ten thousands
		if(value >= 90000)  then:  SetReel nplayer, 1 , 9 : SetScore nplayer,0,9 : value = value - 90000: end if
		if(value >= 80000)  then:  SetReel nplayer, 1 , 8 : SetScore nplayer,0,8 : value = value - 80000: end if
		if(value >= 70000)  then:  SetReel nplayer, 1 , 7 : SetScore nplayer,0,7 : value = value - 70000: end if
		if(value >= 60000)  then:  SetReel nplayer, 1 , 6 : SetScore nplayer,0,6 : value = value - 60000: end if
		if(value >= 50000)  then:  SetReel nplayer, 1 , 5 : SetScore nplayer,0,5 : value = value - 50000: end if
		if(value >= 40000)  then:  SetReel nplayer, 1 , 4 : SetScore nplayer,0,4 : value = value - 40000: end if
		if(value >= 30000)  then:  SetReel nplayer, 1 , 3 : SetScore nplayer,0,3 : value = value - 30000: end if
		if(value >= 20000)  then:  SetReel nplayer, 1 , 2 : SetScore nplayer,0,2 : value = value - 20000: end if
		if(value >= 10000)  then:  SetReel nplayer, 1 , 1 : SetScore nplayer,0,1 : value = value - 10000: end if


	' do thousands
		if(value >= 9000)  then:  SetReel nplayer, 2 , 9 : SetScore nplayer,1,9 : value = value - 9000: end if
		if(value >= 8000)  then:  SetReel nplayer, 2 , 8 : SetScore nplayer,1,8 : value = value - 8000: end if
		if(value >= 7000)  then:  SetReel nplayer, 2 , 7 : SetScore nplayer,1,7 : value = value - 7000: end if
		if(value >= 6000)  then:  SetReel nplayer, 2 , 6 : SetScore nplayer,1,6 : value = value - 6000: end if
		if(value >= 5000)  then:  SetReel nplayer, 2 , 5 : SetScore nplayer,1,5 : value = value - 5000: end if
		if(value >= 4000)  then:  SetReel nplayer, 2 , 4 : SetScore nplayer,1,4 : value = value - 4000: end if
		if(value >= 3000)  then:  SetReel nplayer, 2 , 3 : SetScore nplayer,1,3 : value = value - 3000: end if
		if(value >= 2000)  then:  SetReel nplayer, 2 , 2 : SetScore nplayer,1,2 : value = value - 2000: end if
		if(value >= 1000)  then:  SetReel nplayer, 2 , 1 : SetScore nplayer,1,1 : value = value - 1000: end if

		'do hundreds
		
		if(value >= 900) then: SetReel nplayer, 3 , 9 : SetScore nplayer,2,9 : value = value - 900: end if
		if(value >= 800) then: SetReel nplayer, 3 , 8 : SetScore nplayer,2,8 : value = value - 800: end if
		if(value >= 700) then: SetReel nplayer, 3 , 7 : SetScore nplayer,2,7 : value = value - 700: end if
		if(value >= 600) then: SetReel nplayer, 3 , 6 : SetScore nplayer,2,6 : value = value - 600: end if
		if(value >= 500) then: SetReel nplayer, 3 , 5 : SetScore nplayer,2,5 : value = value - 500: end if
		if(value >= 400) then: SetReel nplayer, 3 , 4 : SetScore nplayer,2,4 : value = value - 400: end if
		if(value >= 300) then: SetReel nplayer, 3 , 3 : SetScore nplayer,2,3 : value = value - 300: end if
		if(value >= 200) then: SetReel nplayer, 3 , 2 : SetScore nplayer,2,2 : value = value - 200: end if
		if(value >= 100) then: SetReel nplayer, 3 , 1 : SetScore nplayer,2,1 : value = value - 100: end if

		'do tens
		if(value >= 90) then: SetReel nplayer, 4 , 9 : SetScore nplayer,3,9 : value = value - 90: end if
		if(value >= 80) then: SetReel nplayer, 4 , 8 : SetScore nplayer,3,8 : value = value - 80: end if
		if(value >= 70) then: SetReel nplayer, 4 , 7 : SetScore nplayer,3,7 : value = value - 70: end if
		if(value >= 60) then: SetReel nplayer, 4 , 6 : SetScore nplayer,3,6 : value = value - 60: end if
		if(value >= 50) then: SetReel nplayer, 4 , 5 : SetScore nplayer,3,5 : value = value - 50: end if
		if(value >= 40) then: SetReel nplayer, 4 , 4 : SetScore nplayer,3,4 : value = value - 40: end if
		if(value >= 30) then: SetReel nplayer, 4 , 3 : SetScore nplayer,3,3 : value = value - 30: end if
		if(value >= 20) then: SetReel nplayer, 4 , 2 : SetScore nplayer,3,2 : value = value - 20: end if
		if(value >= 10) then: SetReel nplayer, 4 , 1 : SetScore nplayer,3,1 : value = value - 10: end if

		'do ones
		if(value >= 9) then: SetReel nplayer, 5 , 9 : SetScore nplayer,4,9 : value = value - 9: end if
		if(value >= 8) then: SetReel nplayer, 5 , 8 : SetScore nplayer,4,8 : value = value - 8: end if
		if(value >= 7) then: SetReel nplayer, 5 , 7 : SetScore nplayer,4,7 : value = value - 7: end if
		if(value >= 6) then: SetReel nplayer, 5 , 6 : SetScore nplayer,4,6 : value = value - 6: end if
		if(value >= 5) then: SetReel nplayer, 5 , 5 : SetScore nplayer,4,5 : value = value - 5: end if
		if(value >= 4) then: SetReel nplayer, 5 , 4 : SetScore nplayer,4,4 : value = value - 4: end if
		if(value >= 3) then: SetReel nplayer, 5 , 3 : SetScore nplayer,4,3 : value = value - 3: end if
		if(value >= 2) then: SetReel nplayer, 5 , 2 : SetScore nplayer,4,2 : value = value - 2: end if
		if(value >= 1) then: SetReel nplayer, 5 , 1 : SetScore nplayer,4,1 : value = value - 1: end if


		end if

	Next
'end if ' USEEM

End Sub 

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
dim LF2 : Set LF2 = New FlipperPolarity
'dim RF2 : Set RF2 = New FlipperPolarity

InitPolarity

Sub InitPolarity()
        dim x, a : a = Array(LF, RF, LF2)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 80
        Next

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

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
		LF2.Object = LeftFlipper2
		LF.EndPoint = EndPointLp2
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
Sub TriggerLF2_Hit() : LF2.Addball activeball : End Sub
Sub TriggerLF2_UnHit() : LF2.PolarityCorrect activeball : End Sub



'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
        dim a : a = Array(LF, RF, LF2)
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
         :                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

                                if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

                                if Enabled then aBall.Velx = aBall.Velx*VelCoef
                                if Enabled then aBall.Vely = aBall.Vely*VelCoef
                        End If

                        'Polarity Correction (optional now)
                        if not IsEmpty(PolarityIn(0) ) then
                                If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
                                dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
        
                                if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
                                'playsound "fx_knocker"
                        End If
                End If
                RemoveBall aBall
        End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
'******************************************************

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
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
        FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
        FlipperTricks LeftFlipper2, LF2Press, LF2Count, LF2EndAngle, LF2State
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
                                        BOT(b).velx = BOT(b).velx / 1.7
                                        BOT(b).vely = BOT(b).vely - 1
                                end If
                        Next
                End If
        Else 
                If Flipper1.currentangle <> EndAngle1 then 
                        EOSNudge1 = 0
                end if
        End If
End Sub

'*****************
' Maths
'*****************

Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
End Function

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

' Used for drop targets and stand up targets
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

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount, LF2Press, LF2Count
dim LFState, RFState, LF2State
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle, LF2EndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.055

LFEndAngle = Leftflipper.endangle
LF2EndAngle = Leftflipper2.endangle
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


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
        RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
        SleevesD.Dampen Activeball
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
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
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

Sub RDampen_Timer()
       Cor.Update
End Sub



'***********************
'     Flipper Logos
'***********************

'Sub UpdateFlipperLogos_Timer
'	Lflipmesh1.RotZ = LeftFlipper.CurrentAngle
'	Rflipmesh1.RotZ = RightFlipper.CurrentAngle
'	Lflipmesh2.RotZ = LeftFlipper2.CurrentAngle
'	Rflipmesh2.Rotz = Flipper5.CurrentAngle
'	FlipperLSh.RotZ = LeftFlipper.currentangle
'	FlipperRSh.RotZ = RightFlipper.currentangle
'	FlipperLSh1.RotZ = LeftFlipper2.CurrentAngle
'	FlipperRSh1.RotZ = Flipper5.CurrentAngle
'	Lflipmesh001.RotZ = LeftFlipperMirror1.CurrentAngle
'	Lflipmesh002.RotZ = LeftFlipperMirror2.CurrentAngle
'	Rflipmesh001.RotZ = RightFlipperMirror.CurrentAngle
'	Rflipmesh002.RotZ = RightFlipperMirror2.CurrentAngle
'End Sub


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
Dim DT1, DT2, DT3, DT4, DT5

Set DT1 = (new DropTarget)(sw1, sw1a, sw1p, 1, 0, false)
Set DT2 = (new DropTarget)(sw2, sw2a, sw2p, 2, 0, false)
Set DT3 = (new DropTarget)(sw3, sw3a, sw3p, 3, 0, false)
Set DT4 = (new DropTarget)(sw4, sw4a, sw4p, 4, 0, false)
Set DT5 = (new DropTarget)(sw5, sw5a, sw5p, 5, 0, false)


Dim DTArray
DTArray = Array(DT1, DT2, DT3, DT4, DT5)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 1 'Set to 0 to disable bricking, 1 to enable bricking
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound
Const DTHitSound = "Target_Hit_1"        'Drop Target Hit sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


Sub ResetDrops(enabled)
     if enabled then
		DTRaise 1
		DTRaise 2
		DTRaise 3
		DTRaise 4
		DTRaise 5
		PlaySoundAt SoundFX(DTResetSound,DOFDropTargets), BulbFil13
		DOF 135,DOFPulse
		DTGI.Intensity = 5
     end if
End Sub



'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

'	PlayTargetSound
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
			if UsingROM then 
				controller.Switch(Switchid) = 1
			else 
				DTAction switchid
			end if
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
			DTAnimate = -2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		if UsingROM then controller.Switch(Switchid) = 0

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

Sub DTAction(switchid)
	Select Case switchid
'		Case 1:
'			Addscore 1000
'			ShadowDT(0).visible=False
'		Case 2:
'			Addscore 1000
'			ShadowDT(1).visible=False
'		Case 3:
'			Addscore 1000
'			ShadowDT(2).visible=False
	End Select
End Sub


'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************


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


DTAnim.ENABLED = True
dtanim.interval = 10

Sub DTAnim_Timer()
	DoDTAnim
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
BumperSoundFactor = 4.25                                                                                                'volume multiplier; must not be zero
KnockerSoundLevel = 1                                                                                                         'volume level; range [0, 1]

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
DTSoundLevel = 0.25                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                                                                      'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                                                                      'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 0.7                                                                                                'volume level; range [0, 1]
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
    tmp = tableobj.y * 2 / captfant.height-1

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
    tmp = tableobj.x * 2 / captfant.width-1

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
			If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
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



'==========================================================================================================================================
'============================================================= START OF HIGH SCORES ROUTINES =============================================================
'==========================================================================================================================================
'
'ADD LINE TO TABLE_KEYDOWN SUB WITH THE FOLLOWING:    If HSEnterMode Then HighScoreProcessKey(keycode) AFTER THE STARTGAME ENTRY
'ADD: And Not HSEnterMode=true TO IF KEYCODE=STARTGAMEKEY
'TO SHOW THE SCORE ON POST-IT ADD LINE AT RELEVENT LOCATION THAT HAS:  UpdatePostIt
'TO INITIATE ADDING INITIALS ADD LINE AT RELEVENT LOCATION THAT HAS:  HighScoreEntryInit()
'ADD THE FOLLOWING LINES TO TABLE_INIT TO SETUP POSTIT
'	if HSA1="" then HSA1=25
'	if HSA2="" then HSA2=25
'	if HSA3="" then HSA3=25
'	UpdatePostIt
'ADD HSA1, HSA2 AND HSA3 TO SAVE AND LOAD VALUES FOR TABLE
'ADD A TIMER NAMED HighScoreFlashTimer WITH INTERVAL 100 TO TABLE
'SET HSSSCOREX BELOW TO WHATEVER VARIABLE YOU USE FOR HIGH SCORE.
'ADD OBJECTS TO PLAYFIELD (EASIEST TO JUST COPY FROM THIS TABLE)
'IMPORT POST-IT IMAGES


Dim HSA1, HSA2, HSA3
Dim HSEnterMode, hsLetterFlash, hsEnteredDigits(3), hsCurrentDigit, hsCurrentLetter
Dim HSArray  
Dim HSScoreM,HSScore100k, HSScore10k, HSScoreK, HSScore100, HSScore10, HSScore1 	'Define 6 different score values for each reel to use
HSArray = Array("Postit0","postit1","postit2","postit3","postit4","postit5","postit6","postit7","postit8","postit9","postitBL","postitCM","Tape")
Const hsFlashDelay = 4

' ***********************************************************
'  HiScore DISPLAY 
' ***********************************************************

Sub UpdatePostIt
	dim tempscore
	hisc =	HiScore
	TempScore = HiScore
	HSScore1 = 0
	HSScore10 = 0
	HSScore100 = 0
	HSScoreK = 0
	HSScore10k = 0
	HSScore100k = 0
	HSScoreM = 0
	if len(TempScore) > 0 Then
		HSScore1 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore10 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore100 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScoreK = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore10k = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore100k = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScoreM = cint(right(Tempscore,1))
	end If
	Pscore6.image = HSArray(HSScoreM):If HiScore<1000000 Then PScore6.image = HSArray(10)
	Pscore5.image = HSArray(HSScore100K):If HiScore<100000 Then PScore5.image = HSArray(10)
	PScore4.image = HSArray(HSScore10K):If HiScore<10000 Then PScore4.image = HSArray(10)
	PScore3.image = HSArray(HSScoreK):If HiScore<1000 Then PScore3.image = HSArray(10)
	PScore2.image = HSArray(HSScore100):If HiScore<100 Then PScore2.image = HSArray(10)
	PScore1.image = HSArray(HSScore10):If HiScore<10 Then PScore1.image = HSArray(10)
	PScore0.image = HSArray(HSScore1):If HiScore<1 Then PScore0.image = HSArray(10)
	if HiScore<1000 then
		PComma.image = HSArray(10)
	else
		PComma.image = HSArray(11)
	end if
	if HiScore<1000000 then
		PComma2.image = HSArray(10)
	else
		PComma2.image = HSArray(11)
	end if
'	if showhisc=1 and showhiscnames=1 then
'		for each objekt in hiscname:objekt.visible=1:next
		HSName1.image = ImgFromCode(HSA1, 1)
		HSName2.image = ImgFromCode(HSA2, 2)
		HSName3.image = ImgFromCode(HSA3, 3)
'	  else
'		for each objekt in hiscname:objekt.visible=0:next
'	end if
End Sub

Function ImgFromCode(code, digit)
	Dim Image
	if (HighScoreFlashTimer.Enabled = True and hsLetterFlash = 1 and digit = hsCurrentLetter) then
		Image = "postitBL"
	elseif (code + ASC("A") - 1) >= ASC("A") and (code + ASC("A") - 1) <= ASC("Z") then
		Image = "postit" & chr(code + ASC("A") - 1)
	elseif code = 27 Then
		Image = "PostitLT"
    elseif code = 0 Then
		image = "PostitSP"
    Else
      msgbox("Unknown display code: " & code)
	end if
	ImgFromCode = Image
End Function

Sub HighScoreEntryInit()
	HSA1=0:HSA2=0:HSA3=0
	HSEnterMode = True
	hsCurrentDigit = 0
	hsCurrentLetter = 1:HSA1=1
	HighScoreFlashTimer.Interval = 250
	HighScoreFlashTimer.Enabled = True
	hsLetterFlash = hsFlashDelay
End Sub

Sub HighScoreFlashTimer_Timer()
	hsLetterFlash = hsLetterFlash-1
	UpdatePostIt
	If hsLetterFlash=0 then 'switch back
		hsLetterFlash = hsFlashDelay
	end if
End Sub


' ***********************************************************
'  HiScore ENTER INITIALS 
' ***********************************************************

Sub HighScoreProcessKey(keycode)
    If keycode = LeftFlipperKey Then
		hsLetterFlash = hsFlashDelay
		Select Case hsCurrentLetter
			Case 1:
				HSA1=HSA1-1:If HSA1=-1 Then HSA1=26 'no backspace on 1st digit
				UpdatePostIt
			Case 2:
				HSA2=HSA2-1:If HSA2=-1 Then HSA2=27
				UpdatePostIt
			Case 3:
				HSA3=HSA3-1:If HSA3=-1 Then HSA3=27
				UpdatePostIt
		 End Select
    End If

	If keycode = RightFlipperKey Then
		hsLetterFlash = hsFlashDelay
		Select Case hsCurrentLetter
			Case 1:
				HSA1=HSA1+1:If HSA1>26 Then HSA1=0
				UpdatePostIt
			Case 2:
				HSA2=HSA2+1:If HSA2>27 Then HSA2=0
				UpdatePostIt
			Case 3:
				HSA3=HSA3+1:If HSA3>27 Then HSA3=0
				UpdatePostIt
		 End Select
	End If
	
    If keycode = StartGameKey Then
		Select Case hsCurrentLetter
			Case 1:
				hsCurrentLetter=2 'ok to advance
				HSA2=HSA1 'start at same alphabet spot
'				EMReelHSName1.SetValue HSA1:EMReelHSName2.SetValue HSA2
			Case 2:
				If HSA2=27 Then 'bksp
					HSA2=0
					hsCurrentLetter=1
				Else
					hsCurrentLetter=3 'enter it
					HSA3=HSA2 'start at same alphabet spot
				End If
			Case 3:
				If HSA3=27 Then 'bksp
					HSA3=0
					hsCurrentLetter=2
				Else
					savehs 'enter it
					HighScoreFlashTimer.Enabled = False
					HSEnterMode = False
				End If
		End Select
		UpdatePostIt
    End If
End Sub

sub savehs
    savevalue "CaptFan", "hiscore", hisc
    savevalue "CaptFan", "score1", Statscores(1)
    savevalue "CaptFan", "score2", Statscores(2)
    savevalue "CaptFan", "score3", Statscores(3)
    savevalue "CaptFan", "score4", Statscores(4)
	savevalue "CaptFan", "DTcount", DTcount
	savevalue "CaptFan", "hsa1", HSA1
	savevalue "CaptFan", "hsa2", HSA2
	savevalue "CaptFan", "hsa3", HSA3
end sub

sub loadhs
    dim temp
    temp = LoadValue("CaptFan", "hiscore")
    If (temp <> "") then hisc = CDbl(temp)
    temp = LoadValue("CaptFan", "score1")
    If (temp <> "") then Statscores(1) = CDbl(temp)
    temp = LoadValue("CaptFan", "score2")
    If (temp <> "") then Statscores(2) = CDbl(temp)
    temp = LoadValue("CaptFan", "score3")
    If (temp <> "") then Statscores(3) = CDbl(temp)
    temp = LoadValue("CaptFan", "score4")
    If (temp <> "") then Statscores(4) = CDbl(temp)

    temp = LoadValue("CaptFan", "DTcount")
    If (temp <> "") then DTcount = CDbl(temp)

    temp = LoadValue("CaptFan", "hsa1")
    If (temp <> "") then HSA1 = CDbl(temp)
    temp = LoadValue("CaptFan", "hsa2")
    If (temp <> "") then HSA2 = CDbl(temp)
    temp = LoadValue("CaptFan", "hsa3")
    If (temp <> "") then HSA3 = CDbl(temp)
end sub


'************************************************************************
'********************* Script for VR Room  ******************************
'************************************************************************



Dim DesktopMode: DesktopMode = captfant.ShowDT

If VRRoom=0 Then
	for each Obj in VRRoom_BaSti : Obj.visible = 0 : next
	for each Obj in VRCabinet : Obj.visible = 0 : next	
	for each obj in VRBackglassGI : Obj.visible = 0 : next
	BGFlashersFast.enabled = False
	VRBGFLTimer.enabled = False
	BeerTimer.enabled = False
	ClockTimer.enabled = False
End If

If VRRoom = 1 Then
	SetBackglass
	center_objects_em
	BGDark.visible = 1
	for each Obj in VRCabinet : Obj.visible = 1 : next
	for each Obj in VRRoom_BaSti : Obj.visible = 1 : next
	for each obj in VRBackglassGI : Obj.visible = 1 : next
	for each Obj in DT_Stuff : Obj.visible = 0 : next
	for each obj in DT_Lights : Obj.state = 0 : next
	for each obj in DT_LightsBlink : Obj.state = 0 : next
	BGFlashersFast.enabled = True
	VRBGFLTimer.enabled = True
	BeerTimer.enabled = True
	ClockTimer.enabled = True
End If

If CabinetMode = 1 Then
	for each Obj in DT_Stuff : Obj.visible = 0 : next
	for each obj in DT_Lights : Obj.state = 0 : next
	for each obj in DT_LightsBlink : Obj.state = 0 : next
	ramp3.visible = 0
	ramp4.visible = 0
	VR_Cab_Lockdownbar.visible = 0
End If

'******************************************************
'*******	Set Up VR Backglass Flashers	*******
'******************************************************
Dim VRBGObj

Sub SetBackglass()

	For Each VRBGObj In VRBackglass
		VRBGObj.x = VRBGObj.x
		VRBGObj.height = - VRBGObj.y + 140
		VRBGObj.y = 20 'adjusts the distance from the backglass towards the user
		VRBGObj.Rotx = -90
	Next

	For Each VRBGObj In VRBackglassReelWall
		VRBGObj.x = VRBGObj.x
		VRBGObj.height = - VRBGObj.y + 140
		VRBGObj.y = -100 'adjusts the distance from the backglass towards the user
		VRBGObj.Rotx = -90
	Next

End Sub


'******************************************************
'*******     VR Backglass Lighting    *******
'******************************************************

Sub FlasherMatch
	If Freeplaynum = 10 Then FlM00.visible = 1 : FlM00A.visible = 1 : FlM00B.visible = 1 Else FlM00.visible = 0 : FlM00A.visible = 0 : FlM00B.visible = 0  End If
	If Freeplaynum = 1 Then FlM10.visible = 1 : FlM10A.visible = 1 : FlM10B.visible = 1 Else FlM10.visible = 0 : FlM10A.visible = 0 : FlM10B.visible = 0 End If
	If Freeplaynum = 2 Then FlM20.visible = 1 : FlM20A.visible = 1 : FlM20B.visible = 1 Else FlM20.visible = 0 : FlM20A.visible = 0 : FlM20B.visible = 0 End If
	If Freeplaynum = 3 Then FlM30.visible = 1 : FlM30A.visible = 1 : FlM30B.visible = 1 Else FlM30.visible = 0 : FlM30A.visible = 0 : FlM30B.visible = 0 End If
	If Freeplaynum = 4 Then FlM40.visible = 1 : FlM40A.visible = 1 : FlM40B.visible = 1 Else FlM40.visible = 0 : FlM40A.visible = 0 : FlM40B.visible = 0 End If
	If Freeplaynum = 5 Then FlM50.visible = 1 : FlM50A.visible = 1 : FlM50B.visible = 1 Else FlM50.visible = 0 : FlM50A.visible = 0 : FlM50B.visible = 0 End If
	If Freeplaynum = 6 Then FlM60.visible = 1 : FlM60A.visible = 1 : FlM60B.visible = 1 Else FlM60.visible = 0 : FlM60A.visible = 0 : FlM60B.visible = 0 End If
	If Freeplaynum = 7 Then FlM70.visible = 1 : FlM70A.visible = 1 : FlM70B.visible = 1 Else FlM70.visible = 0 : FlM70A.visible = 0 : FlM70B.visible = 0 End If
	If Freeplaynum = 8 Then FlM80.visible = 1 : FlM80A.visible = 1 : FlM80B.visible = 1 Else FlM80.visible = 0 : FlM80A.visible = 0 : FlM80B.visible = 0 End If
	If Freeplaynum = 9 Then FlM90.visible = 1 : FlM90A.visible = 1 : FlM90B.visible = 1 Else FlM90.visible = 0 : FlM90A.visible = 0 : FlM90B.visible = 0 End If
End Sub

Sub FlasherBalls
	If Round = 5 Then FlBIP1.visible = 1 : FlBIP1A.visible = 1 Else FlBIP1.visible = 0 : FlBIP1A.visible = 0 End If
	If Round = 4 Then FlBIP2.visible = 1 : FlBIP2A.visible = 1 Else FlBIP2.visible = 0 : FlBIP2A.visible = 0 End If
	If Round = 3 Then FlBIP3.visible = 1 : FlBIP3A.visible = 1 Else FlBIP3.visible = 0 : FlBIP3A.visible = 0 End If
	If Round = 2 Then FlBIP4.visible = 1 : FlBIP4A.visible = 1 Else FlBIP4.visible = 0 : FlBIP4A.visible = 0 End If
	If Round = 1 Then FlBIP5.visible = 1 : FlBIP5A.visible = 1 Else FlBIP5.visible = 0 : FlBIP5A.visible = 0 End If
End Sub

Sub FlasherPlayers
	If MaxPlayers = 1 Then FlPl1.visible = 1 Else FlPl1.visible = 0 End If
	If MaxPlayers = 2 Then FlPl2.visible = 1 Else FlPl2.visible = 0 End If
	If MaxPlayers = 3 Then FlPl3.visible = 1 Else FlPl3.visible = 0 End If
	If MaxPlayers = 4 Then FlPl4.visible = 1 Else FlPl4.visible = 0 End If
End Sub

Sub FlasherCurrentPlayer
	If ActivePlayer = 1 Then : For Each VRObj in VRBGPL1 : VRObj.visible = 1 : next : else : For Each VRObj in VRBGPL1 : VRObj.visible = 0 : next
	If ActivePlayer = 2 Then : For Each VRObj in VRBGPL2 : VRObj.visible = 1 : next : else : For Each VRObj in VRBGPL2 : VRObj.visible = 0 : next
	If ActivePlayer = 3 Then : For Each VRObj in VRBGPL3 : VRObj.visible = 1 : next : else : For Each VRObj in VRBGPL3 : VRObj.visible = 0 : next
	If ActivePlayer = 4 Then : For Each VRObj in VRBGPL4 : VRObj.visible = 1 : next : else : For Each VRObj in VRBGPL4 : VRObj.visible = 0 : next
End Sub

Sub Flasher100k
	If ActivePlayer = 1 Then for each Object in VRBG100k1 : object.visible = 1 : next
	If ActivePlayer = 2 Then for each Object in VRBG100k2 : object.visible = 1 : next
	If ActivePlayer = 3 Then for each Object in VRBG100k3 : object.visible = 1 : next
	If ActivePlayer = 4 Then for each Object in VRBG100k4 : object.visible = 1 : next
	If ActivePlayer = 4 Then for each Object in VRBG100k4 : object.visible = 1 : next
End Sub

Dim Flasherseq1
Dim Flasherseq2
Dim FL1, FL2, Fl3, Fl4

Sub BGFlashersFast_Timer
	Select Case Flasherseq1 'Int(Rnd*6)+1
		Case 1:FL3 = 1 : VRBGFL3lvl = 0.05
		Case 2:FL3 = 2 : Fl4 = 1 : VRBGFL4lvl = 0.05
		Case 3:FL4 = 2
		Case 4:FL1 = 1 : VRBGFL1lvl = 0.05
		Case 5:FL2 = 1 : VRBGFL2lvl = 0.05
		Case 6:FL2 = 2 : FL1 = 2
		Case 8:Fl3 = 1 : VRBGFL3lvl = 0.05
		Case 9:Fl3 = 2
		Case 10:FL4 = 1 : VRBGFL4lvl = 0.05
		Case 11:FL4 = 2
		Case 15:FL3 = 1 : VRBGFL3lvl = 0.05
		Case 16:FL3 = 2
		Case 17:FL2 = 1 : VRBGFL2lvl = 0.05
		Case 18:FL4 = 1 : VRBGFL4lvl = 0.05 : FL2 = 2
		Case 19:FL4 = 2 : FL1 = 1 : VRBGFL1lvl = 0.05
		Case 21:FL1 = 2
		Case 22:Fl3 = 1 : VRBGFL3lvl = 0.05
		Case 24:Fl3 = 2
		Case 26:FL4 = 1 : VRBGFL4lvl = 0.05
		Case 27:FL4 = 2
		Case 28:Fl3 = 1 : VRBGFL3lvl = 0.05
		Case 29:Fl3 = 2 : FL2 = 1 : VRBGFL2lvl = 0.05
		Case 30:FL2 = 2
		Case 34:Fl3 = 1 : VRBGFL3lvl = 0.05 : FL4 = 1 : VRBGFL4lvl = 0.05
		Case 35:Fl3 = 2 : FL4 = 2 : FL1 = 1 : VRBGFL1lvl = 0.05
		Case 37:FL1 = 2
		Case 40:Fl3 = 1 : VRBGFL3lvl = 0.05 : FL2 = 1 : VRBGFL2lvl = 0.05
		Case 41:Fl3 = 2 : FL4 = 1 : VRBGFL4lvl = 0.05 : Fl2 = 2
		Case 42:FL4 = 2
	End Select
	Flasherseq1 = Flasherseq1 + 1
	If Flasherseq1 > 47 Then
	Flasherseq1 = 1
	End if
End Sub


dim VRBGFL1lvl, VRBGFL2lvl, VRBGFL3lvl, VRBGFL4lvl

sub VRBGFLTimer_timer
	If FL1 = 1 Then
		If VRBGFL1lvl < 0.15 Then
			VRBGFL1lvl = 4 * VRBGFL1lvl + 0.01
		Else	
			VRBGFL1lvl = 1.25 * VRBGFL1lvl + 0.01
		End If
		BGBulb022.opacity = 15 * VRBGFL1lvl^2
		BGBulb033.opacity = 15 * VRBGFL1lvl^2
		BGBulb034.opacity = 15 * VRBGFL1lvl^2
		BGBulb036.opacity = 35 * VRBGFL1lvl^1.5
		BGBulb035.opacity = 15 * VRBGFL1lvl
		If VRBGFL1lvl > 1 Then
			VRBGFL1lvl = 1
			FL1 = 0
		End If
	End If
	If FL1 = 2 Then
		VRBGFL1lvl = 0.8 * VRBGFL1lvl - 0.01
		BGBulb022.opacity = 15 * VRBGFL1lvl
		BGBulb033.opacity = 15 * VRBGFL1lvl
		BGBulb034.opacity = 15 * VRBGFL1lvl
		BGBulb036.opacity = 35 * VRBGFL1lvl^2
		BGBulb033.opacity = 15 * VRBGFL1lvl^2
		if VRBGFL1lvl < 0 then 
			VRBGFL1lvl = 0
			FL1 = 0
		End If
	End if

	If FL2 = 1 Then
		If VRBGFL2lvl < 0.15 Then
			VRBGFL2lvl = 4 * VRBGFL2lvl + 0.01
		Else	
			VRBGFL2lvl = 1.25 * VRBGFL2lvl + 0.01
		End If
		BGBulb037.opacity = 20 * VRBGFL2lvl^2.5
		BGBulb038.opacity = 20 * VRBGFL2lvl^2.5
		BGBulb039.opacity = 20 * VRBGFL2lvl^2.5
		BGBulb023.opacity = 45 * VRBGFL2lvl^2
		BGBulb040.opacity = 20 * VRBGFL2lvl^1.5
		If VRBGFL2lvl > 1 Then
			VRBGFL2lvl = 1
			FL2 = 0
		End If
	End If
	If FL2 = 2 Then
		VRBGFL2lvl = 0.8 * VRBGFL2lvl - 0.01
		BGBulb037.opacity = 20 * VRBGFL2lvl^2
		BGBulb038.opacity = 20 * VRBGFL2lvl^2
		BGBulb039.opacity = 20 * VRBGFL2lvl^2
		BGBulb023.opacity = 45 * VRBGFL2lvl^3
		BGBulb040.opacity = 20 * VRBGFL2lvl^3
		if VRBGFL2lvl < 0 then 
			VRBGFL2lvl = 0
			FL2 = 0
		End If
	End if

	If FL3 = 1 Then
		If VRBGFL3lvl < 0.15 Then
			VRBGFL3lvl = 4 * VRBGFL3lvl + 0.01
		Else	
			VRBGFL3lvl = 1.25 * VRBGFL3lvl + 0.01
		End If
		BGBulb041.opacity = 20 * VRBGFL3lvl^2.5
		BGBulb042.opacity = 20 * VRBGFL3lvl^2.5
		BGBulb043.opacity = 20 * VRBGFL3lvl^2.5
		BGBulb024.opacity = 45 * VRBGFL3lvl^2
		BGBulb044.opacity = 20 * VRBGFL3lvl^1.5
		If VRBGFL3lvl > 1 Then
			VRBGFL3lvl = 1
			Fl3 = 0
		End If
	End If
	If FL3 = 2 Then
		VRBGFL3lvl = 0.8 * VRBGFL3lvl - 0.01
		BGBulb041.opacity = 20 * VRBGFL3lvl
		BGBulb042.opacity = 20 * VRBGFL3lvl
		BGBulb043.opacity = 20 * VRBGFL3lvl
		BGBulb024.opacity = 45 * VRBGFL3lvl^2
		BGBulb044.opacity = 20 * VRBGFL3lvl^2
		if VRBGFL3lvl < 0 then 
			VRBGFL3lvl = 0
			FL3 = 0
		End If
	End if

	If Fl4 = 1 Then
		If VRBGFL4lvl < 0.15 Then
			VRBGFL4lvl = 4 * VRBGFL4lvl + 0.01
		Else	
			VRBGFL4lvl = 1.25 * VRBGFL4lvl + 0.01
		End If
		BGBulb046.opacity = 20 * VRBGFL4lvl^2.5
		BGBulb047.opacity = 20 * VRBGFL4lvl^2.5
		BGBulb048.opacity = 20 * VRBGFL4lvl^2.5
		BGBulb032.opacity = 45 * VRBGFL4lvl^2
		BGBulb045.opacity = 20 * VRBGFL4lvl^1.5
		If VRBGFL4lvl > 1 Then
			VRBGFL4lvl = 1
			Fl4 = 0
		End If
	End If
	If Fl4 = 2 Then
		VRBGFL4lvl = 0.8 * VRBGFL4lvl - 0.01
		BGBulb046.opacity = 20 * VRBGFL4lvl
		BGBulb047.opacity = 20 * VRBGFL4lvl
		BGBulb048.opacity = 20 * VRBGFL4lvl
		BGBulb032.opacity = 45 * VRBGFL4lvl^2
		BGBulb045.opacity = 20 * VRBGFL4lvl^2
		if VRBGFL4lvl < 0 then 
			VRBGFL4lvl = 0
			Fl4 = 0
		End If
	End if
end sub

'BG Player Lights Start Sequence
dim FlasherSeq3
Sub BGStartTimer_timer
	Select Case FlasherSeq3
		Case 1:for each Object in VRBGPL1 : object.visible = 1 : Next : 
		Case 2:for each Object in VRBGPL2 : object.visible = 1 : Next : for each Object in VRBGPL1 : object.visible = 0 : Next
		Case 3:for each Object in VRBGPL3 : object.visible = 1 : Next : for each Object in VRBGPL2 : object.visible = 0 : Next
		Case 4:for each Object in VRBGPL4 : object.visible = 1 : Next : for each Object in VRBGPL3 : object.visible = 0 : Next
		Case 5:for each Object in VRBGPL1 : object.visible = 1 : Next : for each Object in VRBGPL4 : object.visible = 0 : Next
		Case 6:
		Case 9:for each Object in VRBGPL2 : object.visible = 1 : Next : for each Object in VRBGPL1 : object.visible = 0 : Next
		Case 10:for each Object in VRBGPL3 : object.visible = 1 : Next : for each Object in VRBGPL2 : object.visible = 0 : Next
		Case 11:for each Object in VRBGPL4 : object.visible = 1 : Next : for each Object in VRBGPL3 : object.visible = 0 : Next
		Case 12:for each Object in VRBGPL4 : object.visible = 0 : Next

	End Select	
	Flasherseq3 = Flasherseq3 + 1
	If Flasherseq3 > 13 Then
		Flasherseq3 = 1
		BGStartTimer.enabled = False
	End if
End Sub



' ***** Beer Bubble Code - Rawd *****

Sub BeerTimer_Timer()

Randomize(21)
BeerBubble1.z = BeerBubble1.z + Rnd(1)*0.5
if BeerBubble1.z > -771 then BeerBubble1.z = -955
BeerBubble2.z = BeerBubble2.z + Rnd(1)*1
if BeerBubble2.z > -768 then BeerBubble2.z = -955
BeerBubble3.z = BeerBubble3.z + Rnd(1)*1
if BeerBubble3.z > -768 then BeerBubble3.z = -955
BeerBubble4.z = BeerBubble4.z + Rnd(1)*0.75
if BeerBubble4.z > -774 then BeerBubble4.z = -955
BeerBubble5.z = BeerBubble5.z + Rnd(1)*1
if BeerBubble5.z > -771 then BeerBubble5.z = -955
BeerBubble6.z = BeerBubble6.z + Rnd(1)*1
if BeerBubble6.z > -774 then BeerBubble6.z = -955
BeerBubble7.z = BeerBubble7.z + Rnd(1)*0.8
if BeerBubble7.z > -768 then BeerBubble7.z = -955
BeerBubble8.z = BeerBubble8.z + Rnd(1)*1
if BeerBubble8.z > -771 then BeerBubble8.z = -955
End Sub



' ***************** VR Clock code below - THANKS RASCAL ******************
Dim CurrentMinute ' for VR clock 
' VR Clock code below....
Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************


'Original Table (1.2) by Dozer
'2.0   - Leojreimroc 	- Added VRRoom with mirrored backglass
'						- Implemented NFozzy Physics
'						- Implemented Fleep sound package
'						- New Playfield and Plastics (Courtesy of Rajo Joey) 
'						- Changed Apron
'						- Added rail in top right lane, and a few missing pegs on left playfield
'						- Added playfield holes under triggers and drop Targets
'						- Replaced Flippers	and adjusted flippers
'						- Fixed issue with drop targets not reseting properly
'						- Changed Bonus Light behaviour
'WIP9					- Fixed a few image that were apppearing in desktop mode and reduced file Size
'WIP10					- Physic tweaks. Adjusted target height and plunger. New Apron.  Fixed Cabinet mode code.
'WIP11					- Changed Drop target sounds, Fixed flipper image.  Startup sequence redone.  Reel reset code implemented.  VR Score reset light sequence implemented.
'WIP12					- Fixed Mirror Flippers
'WIP13					- Fixed flipper sound issue, Fixed lights height for VR.  Added and fixed a few lights.
'WIP14					- Fixed Bonus light issue, adjusted cards and credit light.  Adjusted cabinet mode/desktop background lights.
'WIP15					- Fixed upper flipper physics.  Fixed certain lighting issues.  Adjusted backglass reel holes/added walls to hide reflection behind reels.
'2.0.1					- Fixed DOF
'						- Fixed Ball Shadows (Thanks rothbauerw)
'						- Tuned down rubber posts
'2.0.2	- Wylte			- Implemented Dynamic Ball Shadows
'						- Changed name of Sub BallShadowUpdate to BallReflectionUpdate (stole the shadows), DBS, adjusted X code to Rothbauerw's new
'						- put flipper logos and shadows in FrameTimer (removed duplicate updates in UpdateStuff), lowered bumper and post shadow z's to values below ball shadows
'		- Leojreimroc	- Fixes to B2S and a few DOFs
'						- Added high score tape
'						- Further adjustments to drop target GI lights
'						- Adjustments to how cabinet mode works (table should work for cabinet mode users out of the box) Turning Cabinet mode to 1 just now just takes off the rails.
