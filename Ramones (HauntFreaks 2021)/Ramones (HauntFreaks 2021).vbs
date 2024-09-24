'****************************************************************************************************
'
'	 ███████████     █████████   ██████   ██████    ███████    ██████   █████ ██████████  █████████ 
'	░░███░░░░░███   ███░░░░░███ ░░██████ ██████   ███░░░░░███ ░░██████ ░░███ ░░███░░░░░█ ███░░░░░███
'	 ░███    ░███  ░███    ░███  ░███░█████░███  ███     ░░███ ░███░███ ░███  ░███  █ ░ ░███    ░░░ 
'	 ░██████████   ░███████████  ░███░░███ ░███ ░███      ░███ ░███░░███░███  ░██████   ░░█████████ 
'	 ░███░░░░░███  ░███░░░░░███  ░███ ░░░  ░███ ░███      ░███ ░███ ░░██████  ░███░░█    ░░░░░░░░███
'	 ░███    ░███  ░███    ░███  ░███      ░███ ░░███     ███  ░███  ░░█████  ░███ ░   █ ███    ░███
'	 █████   █████ █████   █████ █████     █████ ░░░███████░   █████  ░░█████ ██████████░░█████████ 
'	░░░░░   ░░░░░ ░░░░░   ░░░░░ ░░░░░     ░░░░░    ░░░░░░░    ░░░░░    ░░░░░ ░░░░░░░░░░  ░░░░░░░░░  
'                                                                                                
'****************************************************************************************************  
'                                                                                         
' Ramones (original 2021)	version 1.0
' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
' 	based on Fireball (Bally 1972)
' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
' Pinball58 - VPX EM original recreation (fireball)
' Arngrim - DOF
' HauntFreaks - Modified Layout, Custom Playfield and Plastics Graphics, Lighting, Object and GI Shadows
' Borgdog - Spinner Logic, Full Rubbers Animation (not just slings), Coding
' Fluffhead35 - Added nfozzy Physics, Fleep sound, Music Changer and Code (left magnasave or left control key)
' Apophis - Fixed Zipper Flipper and Ball Shadows
'
'**************************************************************************************************** 
'*	WARNING WARNING WARNING WARNING
'**
'*	DO NOT DOWNLOAD FROM VPD or ANY UNAUTHORIZED SITE!!!!
'*
'*	Please note that if you downloaded this 
'*	table from ANY UNAUTHORIZED SITE
'*	that you are doing so against the wishes 
'*	of the table authors. 
'*
'*	The admins at UNAUTHORIZED SITES steal and repost
'*	these tables without the authors permission 
'*	and by downloading from their website you
'*	are supporting these actions. 
'*	
'*	Please show support for the table authors
'*	and the community in general by ONLY 
'*	downloading from community supported 
'*	sites such as VPuniverse and VPForums. 
'*
'*	The tables are always free and you will be 
'*	able to receive tech support for any issues
'*	you may have, usually directly from the table author(s).
'*
'*	You will also have access to the latest
'*	updates much quicker and be supporting
'*	the actual table author(s) by doing so. If 
'*	you want to support this community in 
'*	a positive way, all we ask is to download 
'*	these tables from approved websites. 
'*
'*	Thank you! for not being a POS
'****************************************************************************************

Option Explicit
Randomize

'****************************************************************************************
'*******************OPTIONS**************************************************************
'
Dim discspeed: discspeed=50		'set the disc physical max speed, higher for more ball effect
Dim discrotspeed: discrotspeed=6	'set the visual disc rotation speed, units in degrees per timer cycle, higher is faster
Dim Balls: Balls=5		'set balls per game, usual numbers are 3 or 5
Dim freeplay: freeplay=0	'set to 1 to not require coins to play

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8
'
'****************************************************************************************

Const cGameName = "Ramones"   

Dim Zipper
Dim KickIP
Dim Dir
Dim Strong
Dim Player
Dim Score(4), hisc, hiscstate
Dim knockcount
Dim EMR(4)
Dim x, i
Dim Odin
Dim Wotan
Dim Tilt
Dim BarrierPos
Dim Credit
Dim Game
Dim BallN,BallN2,BallN3,BallN4
Dim Replay(4)
Dim ReplayScore1(4)
Dim ReplayScore2(4)
Dim ReplayScore3(4)
Dim TiltCount
Dim Match
Dim PlayerN
Dim AllowStart
Dim Init
Dim BallsNTot
Dim BallIP
Dim PUP
Dim OWBalls
Dim LButton
Dim RButton

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

ExecuteGlobal GetTextFile("core.vbs") : If Err Then MsgBox "Can't open ""core.vbs"""

If Ramones.ShowDT = false then
	EmReel1.Visible = false
	EMReel2.Visible = false
	EMReel3.Visible = false
	EMReel4.Visible = false
	P1up.Visible = false
	P2up.Visible = false
	P3up.Visible = false
	P4up.Visible = false
	CreditsOsd.Visible = false
	CreditCount.Visible = false
	PlayerOsd.Visible = false
	PlayerPlay.Visible = false
	CanPlay.Visible = false 
	PlayerNum.Visible = false 
	BallsOsd.Visible = false 
	BallCount.Visible = false 
	m0.Visible = false : m1.Visible = false : m2.Visible = false : m3.Visible = false : m4.Visible = false : m5.Visible = false : m6.Visible = false : m7.Visible = false : m8.Visible = false : m9.Visible = false
	TILTtb.Visible = false 
	GameOvertb.Visible = false
	Light43.state=1
	Light44.state=1
End If

'********** Table Init ***********

Sub Ramones_Init
	LoadEM
	Kicker3.CreateBall
	Kicker3.Kick 90, 1
	Replay(1) = 1 : Replay(2) = 1 : Replay(3) = 1 : Replay(4) = 1 
	Game = 0
	match=0
	credit=0
	hisc=20000
	HSA1=4
	HSA2=15
	HSA3=7
	loadhs
	UpdatePostIt
	CreditCount.text= (Credit)
	AllowStart = 1
	Set EMR(1) = EMReel1
	Set EMR(2) = EMReel2
	Set EMR(3) = EMReel3
	Set EMR(4) = EMReel4
	ReplayScore1(1) = 56000 : ReplayScore2(1) = 70000 : ReplayScore3(1) = 82000
	ReplayScore1(2) = 56000 : ReplayScore2(2) = 70000 : ReplayScore3(2) = 82000
	ReplayScore1(3) = 56000 : ReplayScore2(3) = 70000 : ReplayScore3(3) = 82000
    ReplayScore1(4) = 56000 : ReplayScore2(4) = 70000 : ReplayScore3(4) = 82000
	Zipper=0
	BarrierPos=0
	OWBalls=0
	Wall80.IsDropped=true
	Wall81.IsDropped=true
	Wall90.IsDropped=true
	Wall91.IsDropped=true
	FlipperLSh.visible = true
	FlipperRSh.visible = true
	FlipperLSh1.visible = false
	FlipperRSh1.visible = false
End Sub

Sub Ramones_Exit()
	Savehs
	flipoff
	If B2SOn Then Controller.stop
End Sub

sub savehs
    savevalue "Ramones", "credit", credit
    savevalue "Ramones", "hiscore", hisc
    savevalue "Ramones", "match", match
    savevalue "Ramones", "score1", score(1)
    savevalue "Ramones", "score2", score(2)
    savevalue "Ramones", "score3", score(3)
    savevalue "Ramones", "score4", score(4)
'	savevalue "Ramones", "balls", balls
	savevalue "Ramones", "hsa1", HSA1
	savevalue "Ramones", "hsa2", HSA2
	savevalue "Ramones", "hsa3", HSA3
'	savevalue "Ramones", "freeplay", freeplay

end sub

sub loadhs
    dim temp
	temp = LoadValue("Ramones", "credit")
    If (temp <> "") then credit = CDbl(temp)
    temp = LoadValue("Ramones", "hiscore")
    If (temp <> "") then hisc = CDbl(temp)
    temp = LoadValue("Ramones", "match")
    If (temp <> "") then match = CDbl(temp)
    temp = LoadValue("Ramones", "score1")
    If (temp <> "") then score(1) = CDbl(temp)
    temp = LoadValue("Ramones", "score2")
    If (temp <> "") then score(2) = CDbl(temp)
    temp = LoadValue("Ramones", "score3")
    If (temp <> "") then score(3) = CDbl(temp)
    temp = LoadValue("Ramones", "score4")
    If (temp <> "") then score(4) = CDbl(temp)
'    temp = LoadValue("Ramones", "balls")
'    If (temp <> "") then balls = CDbl(temp)
    temp = LoadValue("Ramones", "hsa1")
    If (temp <> "") then HSA1 = CDbl(temp)
    temp = LoadValue("Ramones", "hsa2")
    If (temp <> "") then HSA2 = CDbl(temp)
    temp = LoadValue("Ramones", "hsa3")
    If (temp <> "") then HSA3 = CDbl(temp)
'    temp = LoadValue("Ramones", "freeplay")
'    If (temp <> "") then freeplay = CDbl(temp)
end sub

Sub Ramones_MusicDone()
	PlayNextSong
End Sub

'*********************************

'********** InGame Updates ***********

Sub flipperTimer_timer()
	If Zipper=0 Then 
		FlipperL.RotY = LeftFlipper.CurrentAngle+90
		FlipperR.RotY = RightFlipper.CurrentAngle+90
		FlipperLSh.RotZ = LeftFlipper.CurrentAngle
		FlipperRSh.RotZ = RightFlipper.CurrentAngle
	  Else
		FlipperL.RotY = LeftFlipper1.CurrentAngle+90
		FlipperR.RotY = RightFlipper1.CurrentAngle+90
		FlipperLSh1.RotZ = LeftFlipper1.CurrentAngle
		FlipperRSh1.RotZ = RightFlipper1.CurrentAngle
	End If
	GWL.RotZ=Gate3.CurrentAngle+15
	GWL1.RotZ=Gate2.CurrentAngle+15
	GWL3.RotZ=Gate4.CurrentAngle
	GWL4.RotZ=Gate6.CurrentAngle
End Sub

Sub Trigger42_Hit
	GWL2.RotZ=70
	GateTimer.enabled=1
End Sub

Sub GateTimer_timer()
	GWL2.RotZ=30
End Sub

'**************************************
'music
'Dim musicNum : Dim musicEnd
'Sub LaunchMusic_Unhit
'	dim xx
'		For each xx in GI:xx.State = 1:  Next '(plastics lights)
'
'FlashLevel9 = 1 : FlasherFlash9_Timer
'
'flasherstate = true


Dim musicNum : musicNum = 0
sub PlayNextSong
EndMusic
If musicNum = 0 Then PlayMusic "Ramones\Ramones01.mp3" End If
If musicNum = 1 Then PlayMusic "Ramones\Ramones02.mp3" End If
If musicNum = 2 Then PlayMusic "Ramones\Ramones03.mp3" End If
If musicNum = 3 Then PlayMusic "Ramones\Ramones04.mp3" End If
If musicNum = 4 Then PlayMusic "Ramones\Ramones05.mp3" End If
'If musicNum = 5 Then PlayMusic "Ramones/Ramones06.mp3" End If
'If musicNum = 6 Then PlayMusic "Ramones/Ramones07.mp3" End If
'If musicNum = 7 Then PlayMusic "Ramones/Ramones08.mp3" End If
'If musicNum = 8 Then PlayMusic "Ramones/Ramones09.mp3" End If
'If musicNum = 9 Then PlayMusic "Ramones/Ramones10.mp3" End If
'If musicNum = 10 Then PlayMusic "Ramones/Ramones11.mp3" End If
'If musicNum = 11 Then PlayMusic "Ramones/Ramones12.mp3" End If
'If musicNum = 12 Then PlayMusic "Ramones/Ramones13.mp3" End If
'If musicNum = 13 Then PlayMusic "Ramones/Ramones14.mp3" End If
'If musicNum = 14 Then PlayMusic "Ramones/Ramones15.mp3" End If
'If musicNum = 15 Then PlayMusic "Ramones/Ramones16.mp3" End If
'If musicNum = 16 Then PlayMusic "Ramones/Ramones17.mp3" End If
'If musicNum = 17 Then PlayMusic "Ramones/Ramones18.mp3" End If
'If musicNum = 18 Then PlayMusic "Ramones/Ramones19.mp3" End If
'If musicNum = 19 Then PlayMusic "Ramones/Ramones20.mp3" End If
'If musicNum = 20 Then PlayMusic "Ramones/Ramones21.mp3" End If
'If musicNum = 21 Then PlayMusic "Ramones/Ramones22.mp3" End If
'If musicNum = 22 Then PlayMusic "Ramones/Ramones23.mp3" End If
'If musicNum = 23 Then PlayMusic "Ramones/Ramones24.mp3" End If
'If musicNum = 24 Then PlayMusic "Ramones/Ramones25.mp3" End If
'If musicNum = 25 Then PlayMusic "Ramones/Ramones26.mp3" End If
'If musicNum = 26 Then PlayMusic "Ramones/Ramones27.mp3" End If
'If musicNum = 27 Then PlayMusic "Ramones/Ramones28.mp3" End If
'If musicNum = 28 Then PlayMusic "Ramones/Ramones29.mp3" End If
'If musicNum = 29 Then PlayMusic "Ramones/Ramones30.mp3" End If
'
musicNum = (musicNum + 1) mod 5
End Sub


'********* Add Score & Replay ****************************************

Sub AddScore(points)

	If Player = 1 Then x = 1 End If
	If Player = 2 Then x = 2 End If
	If Player = 3 Then x = 3 End If
	If Player = 4 Then x = 4 End If

	Score(x) = Score(x) + points

	EMR(x).setvalue(score(x))

	If points=10 then 
		Match=Match+10
		if Match>90 then Match = 0
	end if

	If B2SOn Then
		Controller.B2SSetScorePlayer 1, Score(1)
		Controller.B2SSetScorePlayer 2, Score(2)
		Controller.B2SSetScorePlayer 3, Score(3)
		Controller.B2SSetScorePlayer 4, Score(4)
	End If

	If Score(x) >= ReplayScore1(x) And Replay(x) = 1 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=2
	End If

	If Score(x) >= (ReplayScore1(x)*2) And Replay(x) = 2 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=3
	End If

	If Score(x) >= (ReplayScore1(x)*3) And Replay(x) = 3 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=4
	End If

	If Score(x) >= (ReplayScore2(x)) And Replay(x) = 5 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=6
	End If

	If Score(x) >= (ReplayScore2(x)*2)  And Replay(x) = 6 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=7
	End If

	If Score(x) >= (ReplayScore2(x)*3) And Replay(x) = 7 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=8
	End If

	If Score(x) >= (ReplayScore3(x)) And Replay(x) = 10 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=11
	End If

	If Score(x) >= (ReplayScore3(x)*2) And Replay(x) = 11 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=12
	End If

	If Score(x) >= (ReplayScore3(x)*3) And Replay(x) = 12 Then
		If Credit <25 Then
		Credit = Credit + 1
		DOF 122, DOFOn
		CreditCount.text= (Credit)
		If B2SOn Then
		Controller.B2SSetCredits Credit
		End If
		End If
		'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
		KnockerSolenoid
		Replay(x)=0
	End If

	'**** Bells Sound Score based ***



	If points = 10 And (Score(x) MOD 100)\10 = 0 Then 
		PlaySound SoundFXDOF("bell_100-1000",142,DOFPulse,DOFChimes),0,0.7
		ElseIf points = 1000 Then 
		PlaySound SoundFXDOF("bell_100-1000",142,DOFPulse,DOFChimes),0,0.7
		ElseIf points = 100 Then 
		PlaySound SoundFXDOF("bell_100-1000",142,DOFPulse,DOFChimes),0,0.7
		ElseIf points = 10 Then 
		PlaySound SoundFXDOF("bell_10",141,DOFPulse,DOFChimes)
	End If

	'*********************************

End Sub

'*******************************************************************

'********* Match **************

Sub MatchTimer_timer()
	
	'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
	KnockerSolenoid
	knockcount=knockcount+1
	if knockcount=3 then me.enabled=0
End Sub

Sub CheckMatch
	P1up.text = "" : P2up.text = "" : P3up.text = "" : P4up.text = ""
	If Match = 0 Then m0.text = "00" End If
	If Match = 10 Then m1.text = "10" End If
	If Match = 20 Then m2.text = "20" End If
	If Match = 30 Then m3.text = "30" End If
	If Match = 40 Then m4.text = "40" End If
	If Match = 50 Then m5.text = "50" End If
	If Match = 60 Then m6.text = "60" End If
	If Match = 70 Then m7.text = "70" End If
	If Match = 80 Then m8.text = "80" End If
	If Match = 90 Then m9.text = "90" End If
	If B2SOn Then
		If Match = 0 Then
			Controller.B2SSetMatch 34, 100
			Else
			Controller.B2SSetMatch 34, Match
		End If
		Controller.B2SSetPlayerUp 30, 0
		Controller.B2SSetData "p1",0
		Controller.B2SSetData "p2",0
		Controller.B2SSetData "p3",0
		Controller.B2SSetData "p4",0
	End If
	hiscstate=0
	For i=1 to PlayerN
		if match=(score(i) mod 100) then 
			'PlaySound SoundFXDOF("knocker",109,DOFPulse,DOFKnocker)
			KnockerSolenoid
			If Credit<25 Then 
				Credit = Credit+1 
				DOF 122, DOFOn
				CreditCount.text = (Credit) 
				If B2SOn Then Controller.B2SSetCredits Credit
			End If
	    end if
		if Score(i)>hisc then
			hisc=score(i)
			hiscstate=1
		end if
    next

	if hiscstate=1 then 
		HighScoreEntryInit()
		knockcount=0
		MatchTimer.enabled=1
		UpdatePostIt 
		savehs
	end if

    EndMusic

End Sub


'******************************

'********* Table Keys **********

Sub Ramones_KeyDown(ByVal keycode)

	If keycode = AddCreditKey then
		if Credit<25 and freeplay=0 Then
			'PlaySound "CoinIn"
			Select Case Int(rnd*3)
				Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
				Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
				Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

			End Select
			Credit = Credit+1
			DOF 122, DOFOn
			CreditCount.text= (Credit)
			If B2SOn Then
				Controller.B2SSetCredits Credit
			End If
		  Else
			Playsound "CoinReject"
		End if
	End If

	If keycode = LeftMagnaSave Then PlayNextSong

	If keycode = StartGameKey And (Credit>0 or freeplay=1) And PlayerN <4 And AllowStart = 1 And Not HSEnterMode=true Then
		PlayerN = PlayerN + 1
		if freeplay=0 then Credit = Credit-1
		If Credit < 1 Then DOF 122, DOFOff
		Init = Init + 1
		If Init = 1 Then
		FirstStartTimer.enabled = 1	
		StartupTimer.enabled = 1

		End If
		PlaySound "start"
		PlayNextSong
		CreditCount.text= (Credit)
		Game = 1
		BallN= 5
		BallN2= 5
		BallN3= 5
		BallN4= 5
		If PlayerN = 1 Then 
			BallsNTot = Balls
			If Ramones.ShowDT = true Then 
			EMReel1.visible = true
			EMReel2.visible = false
			EMReel3.visible = false
			EMReel4.visible = false
			End If
			If B2SOn Then 
			Controller.B2SSetData "p1",1
			Controller.B2SSetData "p2",0
			Controller.B2SSetData "p3",0
			Controller.B2SSetData "p4",0
			End If
		End If
		If PlayerN = 2 Then 
			BallsNTot = Balls*2 
			If Ramones.ShowDT = true then 
			EMReel2.visible = true
			EMReel3.visible = false
			EMReel4.visible = false 
			End If
			If B2SOn Then 
			Controller.B2SSetData "p1",1
			Controller.B2SSetData "p2",1
			Controller.B2SSetData "p3",0
			Controller.B2SSetData "p4",0
			End If
		End If
		If PlayerN = 3 Then 
			BallsNTot = Balls*3 
			If Ramones.ShowDT = true then 
			EMReel2.visible = true
			EMReel3.visible = true
			EMReel4.visible = false 
			End If
			If B2SOn Then 
			Controller.B2SSetData "p1",1
			Controller.B2SSetData "p2",1
			Controller.B2SSetData "p3",1
			Controller.B2SSetData "p4",0
			End If
		End If
		If PlayerN = 4 Then
			BallsNTot = Balls*4
			If Ramones.ShowDT = true then 
			EMReel2.visible = true 
			EMReel3.visible = true 
			EMReel4.visible = true 
			End If
			If B2SOn Then 
			Controller.B2SSetData "p1",1
			Controller.B2SSetData "p2",1
			Controller.B2SSetData "p3",1
			Controller.B2SSetData "p4",1
			End If
		End If
		m0.text = "" : m1.text = "" : m2.text = "" : m3.text = "" : m4.text = "" : m5.text = "" : m6.text = "" : m7.text = "" : m8.text = "" : m9.text = ""

		If Replay(1) >1 Or Replay(2) >1 Or Replay(3) >1 Or Replay(4) >1 Then
			Replay(1) = 5 : Replay(2) = 5 : Replay(3) = 5 : Replay(4) = 5 
		End If
		If Replay(1) >5 Or Replay(2) >5 Or Replay(3) >5 Or Replay(4) >5 Then
			Replay(1) = 10 : Replay(2) = 10 : Replay(3) = 10 : Replay(4) = 10
		End If
		GameOvertb.text = ""
		Tilttb.text= ""
		PlayerNum.text = (PlayerN)
		If Player = 0 Then 
			PlayerPlay.text = ""
		  Else
			PlayerPlay.text = (Player)
		End If
		If B2SOn Then
			Controller.B2SSetCredits Credit 
			Controller.B2SSetCanPlay 31, PlayerN
			Controller.B2SSetGameOver 0
			Controller.B2SSetMatch 34,0
		End If
	End If

	If HSEnterMode Then HighScoreProcessKey(keycode)

	If keycode = PlungerKey Then
		Plunger.PullBack
		SoundPlungerPull()
	End If

	If keycode = LeftFlipperKey And Game = 1 And Tilt = False Then
		LeftFlipper.RotateToEnd
		LeftFlipper1.RotateToEnd
		FlipperActivate LeftFlipper, LFPress
		FlipperActivate LeftFlipper1, LFPress01
		'PlaySound SoundFXDOF("fx_flipperup",101,DOFOn,DOFContactors), 0, 1, -0.05, 0.05
		If LeftFlipper.Enabled Then
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
		If LeftFlipper1.Enabled Then
			SoundFlipperUpAttackLeft LeftFlipper1
			RandomSoundFlipperUpLeft LeftFlipper1
		End if
		PlaySound "buzz",-1,0.3,-1
		LButton=1
	End If
    
	If keycode = RightFlipperKey And Game = 1 And Tilt = False Then
		RightFlipper.RotateToEnd
		RightFlipper1.RotateToEnd
		FlipperActivate RightFlipper, RFPress
		FlipperActivate RightFlipper1, RFPress01
		'PlaySound SoundFXDOF("fx_flipperup",102,DOFOn,DOFContactors), 0, 1, 0.05, 0.05
		If RightFlipper.Enabled Then
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
		If RightFlipper1.Enabled Then
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If
		PlaySound "buzz1",-1,0.3,1
		RButton=1
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 2
		SoundNudgeLeft()
		CheckTilt
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		SoundNudgeRight()
		CheckTilt
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		SoundNudgeCenter()
		CheckTilt
	End If 

End Sub

Sub Ramones_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.Fire
		SoundPlungerReleaseBall()
	End If
    
	If keycode = LeftFlipperKey And Game = 1 And Tilt = False Then
		LeftFlipper.RotateToStart
		LeftFlipper1.RotateToStart
		FlipperDeActivate LeftFlipper, LFPress
		FlipperDeActivate LeftFlipper1, LFPress01
		'PlaySound "fx_flipperdown", 0, 1, -0.05, 0.05
		If LeftFlipper.Enabled Then
			If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
				RandomSoundFlipperDownLeft LeftFlipper
			End If
			FlipperLeftHitParm = FlipperUpSoundLevel
		End If
		If LeftFlipper1.Enabled Then
			If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
				RandomSoundFlipperDownLeft LeftFlipper1
			End If
			FlipperLeftHitParm = FlipperUpSoundLevel
		End If
		StopSound "buzz"
		If Tilt = true Then
		FlipOff
		End If
		LButton=0
	End If
    
	If keycode = RightFlipperKey And Game = 1 And Tilt = False Then
		RightFlipper.RotateToStart
		RightFlipper1.RotateToStart
		FlipperDeActivate RightFlipper, RFPress
		FlipperDeActivate RightFlipper1, RFPress01
		'PlaySound "fx_flipperdown", 0, 1, 0.05, 0.05
		If RightFlipper.Enabled Then
			If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
				RandomSoundFlipperDownRight RightFlipper
			End If	
			FlipperRightHitParm = FlipperUpSoundLevel
		End If
		If RightFlipper1.Enabled Then
			If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
				RandomSoundFlipperDownRight RightFlipper1
			End If	
			FlipperRightHitParm = FlipperUpSoundLevel
		End If
		StopSound "buzz1"
		If Tilt = true Then
		FlipOff
		End If
		RButton=0
	End If

End Sub

'****************************

'******** Drain *************

Sub Drain_Hit()
	DOF 120, DOFPulse
	If Player = 1 Then BallN = BallN -1 End If
	If Player = 2 Then BallN2 = BallN2 -1 End If
	If Player = 3 Then BallN3 = BallN3 -1 End If
	If Player = 4 Then BallN4 = BallN4 -1 End If
	BallsNTot = BallsNTot - 1	  
	RandomSoundDrain Drain
	Drain.DestroyBall
	AllowStart = 0
	
	If BallsNTot = 0 Then
		CheckMatch
		CallFlipOff.enabled=1
		Game = 0.5
		StopDisc
		BlinkingOff.enabled=1
		GameOverTimer.enabled = 1
	End If

	If BallsNtot >0 Then 
	NewBall.enabled=1
	NewBallSound.enabled=1
	End If

	'bsTrough.addball me	:EndMusic

End Sub

Sub OWDrain_Hit()
	RandomSoundDrain OWDrain
	OWDrain.Destroyball
	OWBalls=OwBalls-1
	If OWBalls=0 Then
		OWDrain.enabled=0 
	End If
End Sub

Sub BlinkingOff_timer()
	BlinkingSC.enabled=0 : BlinkingSC1.enabled=0 : BlinkingSC2.enabled=0 : BlinkingSC3.enabled=0 : BlinkingSC4.enabled=0
	BlinkingOff.enabled=0
End Sub
	
'***************************

'****** Stop SpinDisc ***************


Sub StopDisc
	mDISC.MotorOn = 0
	stopsound "disc_noise"
	stopsound "disc_noise1"
	stopsound "Disc_Noise2"
End Sub

'*************************************

'*************** Start & Drain Sequential Events *******************

Sub FirstStartTimer_timer()
	PlaySound "fire_init"
	If Odin=1 Then BlinkingOn.enabled=1 End If
	If B2SOn Then
	Controller.B2SStartAnimation "bally1"
	Controller.B2SStartAnimation "bally2"
	End If
	FirstStartTimer.enabled = 0
End Sub

Sub BlinkingOn_timer()
	BlinkingSC.enabled=1
	BlinkingOn.enabled=0
End Sub

Sub	StartupTimer_timer()
	EMR(1).ResetToZero()
	EMR(2).ResetToZero()
	EMR(3).ResetToZero()
	EMR(4).ResetToZero()
	Score(1) = 0
	Score(2) = 0
	Score(3) = 0
	Score(4) = 0
	If B2SOn Then
		Controller.B2SSetScorePlayer 1, Score(1)
		Controller.B2SSetScorePlayer 2, Score(2)
		Controller.B2SSetScorePlayer 3, Score(3)
		Controller.B2SSetScorePlayer 4, Score(4)
	End If
	StartupTimer.enabled = 0
	NewBall.enabled = 1
	NewBallSound.enabled = 1
	End Sub

Sub NewBallSound_timer()
	PlaySound SoundFXDOF("release2",110,DOFPulse,DOFContactors),0,1,0,0.25
	NewBallSound.enabled=0
End Sub

Sub NewBall_timer()
	BallRelease.CreateBall
	BallRelease.Kick 80, 12
	Light23.state=0
	Kicker4.enabled=0
	Light29.state=0 : Light30.state=0 : Light31.state=0 
	Light33.state=0 : Light36.state=0 : Light39.state=0 : Light32.state=0 : Light35.state=0 : Light38.state=0
	If BarrierPos=1 Then BarrierC.enabled=1 End If
	Wall78.IsDropped=false
	Wall79.IsDropped=true
	Wall90.IsDropped=true
	Wall91.IsDropped=true
	If Zipper=1 Then 
		Playsound SoundFXDOF ("unzipflip", 119, DOFPulse, DOFContactors)
		Zipper=0
		ZipTimer.enabled=0
		UnZipTimer.enabled=1
		LeftFlipper1.enabled=0 : RightFlipper1.enabled=0 : LeftFlipper.enabled=1 : RightFlipper.enabled=1
		FlipperLSh.visible = true
		FlipperRSh.visible = true
		FlipperLSh1.visible = false
		FlipperRSh1.visible = false
	End If
	Bumper1.force = 6
	Bumper2.force = 6
	Bumper3.force = 6
	Tilt = false
	Tilttb.text = ""
	If B2SOn Then
	Controller.B2SSetTilt 33,0
	End If
	If PlayerN = 1 Or Player=PlayerN Then
	Player = 1
	Else
	Player = Player + 1
	End If
	PlayerPlay.text = (Player)
	If Player = 1 And BallN = 5 Then BallIP = 1 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 1 And BallN = 4 Then BallIP = 2 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 1 And BallN = 3 Then BallIP = 3 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 1 And BallN = 2 Then BallIP = 4 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 1 And BallN = 1 Then BallIP = 5 : P1up.text = "1UP" : P2up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 1 End If
	If Player = 2 And BallN2 = 5 Then BallIP = 1 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 2 And BallN2 = 4 Then BallIP = 2 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 2 And BallN2 = 3 Then BallIP = 3 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 2 And BallN2 = 2 Then BallIP = 4 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 2 And BallN2 = 1 Then BallIP = 5 : P2up.text = "2UP" : P1up.text = "" : P3up.text = "" : P4up.text = "" : PUP = 2 End If
	If Player = 3 And BallN3 = 5 Then BallIP = 1 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 3 And BallN3 = 4 Then BallIP = 2 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 3 And BallN3 = 3 Then BallIP = 3 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 3 And BallN3 = 2 Then BallIP = 4 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 3 And BallN3 = 1 Then BallIP = 5 : P3up.text = "3UP" : P1up.text = "" : P2up.text = "" : P4up.text = "" : PUP = 3 End If
	If Player = 4 And BallN4 = 5 Then BallIP = 1 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	If Player = 4 And BallN4 = 4 Then BallIP = 2 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	If Player = 4 And BallN4 = 3 Then BallIP = 3 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	If Player = 4 And BallN4 = 2 Then BallIP = 4 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	If Player = 4 And BallN4 = 1 Then BallIP = 5 : P4up.text = "4UP" : P1up.text = "" : P2up.text = "" : P3up.text = "" : PUP = 4 End If
	BallCount.text = (BallIP)
	If B2SOn Then
		Controller.B2SSetPlayerUp 30, PUP
		Controller.B2SSetBallinPlay 32, BallIP
	End If
	NewBall.enabled=0
End Sub

Sub GameOverTimer_timer()
	GameOvertb.text = "GAME OVER"
	Game = 0
	AllowStart = 1
	Init = 0
	PlayerN = 0
	If B2SOn Then 
		Controller.B2SSetGameOver 35,1
	End If
	PlaySound "gameover"
	If B2SOn Then 
		Controller.B2SStopAnimation "bally1"
		Controller.B2SStopAnimation "bally2"
		Controller.B2SSetData "ly",1
		Controller.B2SSetData "ba",1
	End If
	GameOverTimer.enabled = 0
End Sub

'****************************************************************************

'************ Tilt **********************************

Sub TiltTimer_Timer()
	TiltTimer.Enabled = False
End Sub

Sub CheckTilt
	If TiltTimer.Enabled = True And Game = 1 Then 
		TiltCount = TiltCount + 1
		If TiltCount = 3 Then
			Bumper1.force = 0
			Bumper2.force = 0
			Bumper3.force = 0
			Tilt = True
			PlaySound "tilt"
			FlipOff
			Tilttb.text= "TILT"
			Wall90.IsDropped=false
			Wall91.IsDropped=false
			If B2SOn Then
				Controller.B2SSetTilt 33,1
			End If
		End If
	Else
		TiltCount = 0
		TiltTimer.Enabled = True
	End If
End Sub

Sub FlipOff
	If Zipper=0 Then
		LeftFlipper.rotatetostart
		RightFlipper.rotatetostart
	End If
	If Zipper=1 Then 
		LeftFlipper.rotatetostart
		RightFlipper.rotatetostart
		LeftFlipper1.rotatetostart
		RightFlipper1.rotatetostart
		Playsound SoundFXDOF ("unzipflip", 119, DOFPulse, DOFContactors)
		Zipper=0
		ZipTimer.enabled=0
		UnZipTimer.enabled=1
		LeftFlipper1.enabled=0 : RightFlipper1.enabled=0 : LeftFlipper.enabled=1 : RightFlipper.enabled=1
		FlipperLSh.visible = true
		FlipperRSh.visible = true
		FlipperLSh1.visible = false
		FlipperRSh1.visible = false
	End If

	StopSound "buzz"
	StopSound "buzz1"
	StopSound "fldown"
End Sub

'****************************************************

'******* Spinning Disc *********



Dim mDISC
    Set mDISC=New cvpmTurnTable
		mDISC.InitTurnTable Plunder,discspeed
		mDISC.SpinUp=1000
		mDISC.SpinDown=10
		mDISC.CreateEvents"mDISC"

Sub RotationTimer_Timer
	dim temptimer
	if mDisc.speed > 0 Then
		temptimer = int(((1/mDisc.speed)*100)+0.5)
		if temptimer<1 Then	
				RotationTimer.interval = 1
			elseif temptimer>100 then 
				RotationTimer.interval = 100
			else rotationtimer.interval=temptimer
		end if
		disc.RotZ = (disc.RotZ + discrotspeed) MOD 360
	  Else
		RotationTimer.interval = 1
	end If
End Sub


Sub Trigger20_Hit                          
	mTrigger.MagnetOn = 0
	MBAntiGrab.enabled = 1
End Sub

Sub MBAntiGrab_timer()
	mTrigger.MagnetOn = 0
	MBAntiGrab.enabled = 0
End Sub

Sub DiscTriggers_Hit(index)
	Dim mStrong          
	mTrigger.MagnetOn = 1
	mStrong = Int(rnd*8)+1
	Select Case mStrong
	Case 1: mTrigger.InitMagnet GrabM, 9 
	Case 2: mTrigger.InitMagnet GrabM, 8 
	Case 3: mTrigger.InitMagnet GrabM, 7
	Case 4: mTrigger.InitMagnet GrabM, 6
	Case 5: mTrigger.InitMagnet GrabM, 5
	Case 6: mTrigger.InitMagnet GrabM, 10
	Case 7: mTrigger.InitMagnet GrabM, 4
	Case 8: mTrigger.InitMagnet GrabM, 3
	End Select	
End Sub

Sub Trigger34_Hit
	'PlaySound "disc_roll",1
	PlaySoundAt "disc_roll", disc
End Sub

Sub Trigger35_Hit
	StopSound "disc_roll"	
End Sub

Sub Trigger36_Hit
	Dim DiscHop         
	DiscHop = Int(rnd*2)+1
'	Select Case DiscHop
'		Case 1: PlaySound "hop1",0,0.5
'		Case 2: PlaySound "hop2",0,0.5
'	End Select
	Select Case DiscHop
		Case 1: PlaySoundAt "hop1", disc
		Case 2: PlaySoundAt "hop2", disc
	End Select
End Sub

Sub Trigger37_Hit
	StopSound "disc_roll"	
End Sub

'********************************

'******* Hit, Events and Sounds ************

Sub Trigger19_Hit
	If Tilt=false Then AddScore (100) End If
End Sub

Sub Trigger16_Hit
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Trigger17_Hit
	Light32.state=1
	Light35.state=1
	Light38.state=1
	Light33.state=1
	Light36.state=1
	Light39.state=1
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Trigger18_Hit
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Trigger13_Hit
	If Tilt=false Then
	If Light31.state=0 Then 
	AddScore (100) 
	End If
	If Light31.state=1 Then 
	AddScore (1000) 
	End If
    End If
End Sub

Sub Trigger12_Hit
	If Tilt=false Then
	If Light30.state=0 Then 
	AddScore (100) 
	End If
	If Light30.state=1 Then 
	AddScore (1000) 
	End If
    End If
End Sub

Sub Trigger11_Hit
	If Tilt=false Then
		If Light29.state=0 Then 
		AddScore (100) 
		End If
		If Light29.state=1 Then 
		AddScore (1000) 
		End If
    End If
	If Wall78.IsDropped=true Then Trigger40.enabled=1 End If
End Sub

Sub Trigger40_Hit
	If BarrierPos=1 Then
	BarrierC.enabled=1 
	Wall78.IsDropped=false
	Wall79.IsDropped=true
	End If
End Sub

Sub Trigger40_UnHit
	Trigger40.enabled=0
End Sub

Sub Trigger43_Hit
	DOF 121, DOFPulse
End Sub

Sub Trigger14_Hit
	If Tilt=false Then AddScore (100) End If
End Sub

Sub Trigger15_Hit
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Trigger9_Hit
	DOF 117, DOFPulse
	If Tilt=false Then AddScore (1000) End If
	If Tilt=true Then Kicker4.enabled=0 End If
End Sub

Sub Trigger10_Hit
	DOF 118, DOFPulse
	If Tilt=false Then AddScore (1000) End If
End Sub

Sub Target1_Hit
	DOF 108, DOFPulse
	If Tilt=false Then
	AddScore (1000)
	If Odin=1 Then OdinKick.enabled=1 End If
	If Wotan=1 Then WotanKick.enabled=1 End If
	If BarrierPos=0 Then 
	Barrier.enabled=1 
	Wall78.IsDropped=true
	Wall79.IsDropped=false
	End If
	End If
End Sub

Sub Barrier_timer()
	Primitive15.RotY = Primitive15.RotY-10
	If Primitive15.RotY=100 Then Barrier.enabled=0 : Primitive15.RotY=100 : PlaySound "barrier_click",0,1,1 End If
	BarrierPos=1
End Sub

Sub BarrierC_timer()
	Primitive15.RotY = Primitive15.RotY+10
	If Primitive15.RotY=140 Then BarrierC.enabled=0 : Primitive15.RotY=140  : PlaySound "barrier_click",0,1,1 End If
	BarrierPos=0
End Sub

Sub Kicker1_Hit
	'PlaySound "kicker_enter_center", 0, 0.3
	PlaySoundAt "kicker_enter_center", Kicker1
	If OWBalls=0 Then ReleaseTimer.enabled=1 End If
	If OWBalls>0 Then
	OwBalls=OWBalls-1 
	CheckOWBalls
	End If
	BlinkingSC.enabled=1
	Odin=1
End Sub

Sub ReleaseTimer_timer()
	'PlaySound "ballrelease",0,1,0,0.25
	RandomSoundBallRelease BallRelease
	BallRelease.CreateBall
	BallRelease.Kick 80, 12
	ReleaseTimer.enabled=0
End Sub

Sub CheckOWBalls
	If OWBalls=0 Then OWDrain.enabled=0 End If
End Sub

Sub BlinkingSC_timer()
	Light28.state=0
	Light27.state=0
	Light26.state=0
	Light25.state=0
	Light24.state=1
	BlinkingSC1.enabled=1
	BlinkingSC.enabled=0
End Sub
	
Sub BlinkingSC1_timer()
	Light24.state=0
	Light25.state=1
	BlinkingSC2.enabled=1
	BlinkingSC1.enabled=0
End Sub

Sub BlinkingSC2_timer()
	Light25.state=0
	Light26.state=1
	BlinkingSC3.enabled=1
	BlinkingSC2.enabled=0
End Sub

Sub BlinkingSC3_timer()
	Light26.state=0
	Light27.state=1
	BlinkingSC4.enabled=1
	BlinkingSC3.enabled=0
End Sub

Sub BlinkingSC4_timer()
	Light27.state=0
	Light28.state=1
	BlinkingSC.enabled=1
	BlinkingSC4.enabled=0
End Sub

Sub Kicker1_UnHit
	'PlaySound "popper_ball",0,.75,0,0.25
	PlaySoundAt "popper_ball", Kicker1
End Sub

Sub Kicker2_Hit
	'PlaySound "kicker_enter_center", 0,0.3
	PlaySoundAt "kicker_enter_center", Kicker2
	If OWBalls=0 Then ReleaseTimer.enabled=1 End If
	If OWBalls>0 Then
	OWBalls=OwBalls-1
	CheckOWballs
	End If
	Wotan=1
End Sub

Sub Kicker2_UnHit
	'PlaySound "popper_ball",0,.75,0,0.25
	PlaySoundAt "popper_ball", Kicker2
End Sub

Sub TrigMushL_Hit
	DOF 114, DOFPulse
	Dim MushLSound         
	MushLSound = Int(rnd*3)+1
	Select Case MushLSound
	Case 1: PlaySoundAt "flip_hit_1", TrigMushL
	Case 2: PlaySoundAt "flip_hit_2", TrigMushL
	Case 2: PlaySoundAt "flip_hit_3", TrigMushL
	End Select	
	If Tilt=false Then
	If Zipper=1 Then 
		Playsound SoundFXDOF ("unzipflip", 119, DOFPulse, DOFContactors)
		Zipper=0
		ZipTimer.enabled=0
		UnZipTimer.enabled=1
		LeftFlipper1.enabled=0 : RightFlipper1.enabled=0 : LeftFlipper.enabled=1 : RightFlipper.enabled=1
		FlipperLSh.visible = true
		FlipperRSh.visible = true
		FlipperLSh1.visible = false
		FlipperRSh1.visible = false
	End If
	TmushL.TransZ = 6
	me.timerenabled = 1
	If Wotan = 1 Then WotanKick.enabled=1 End If
	AddScore (100)
	Light31.state=1
	End If
End Sub

Sub TrigMushL_timer()
	TmushL.TransZ = -6
	me.timerenabled = 0
End Sub

Sub TrigMushC_Hit
	DOF 115, DOFPulse
	Dim MushCSound         
	MushCSound = Int(rnd*3)+1
	Select Case MushCSound
		Case 1: PlaySoundAt "flip_hit_1", TrigMushC
		Case 2: PlaySoundAt "flip_hit_2", TrigMushC
		Case 2: PlaySoundAt "flip_hit_3", TrigMushC
	End Select
	If Tilt=false Then
	If Zipper=0 Then
		Playsound SoundFXDOF ("zipflip", 119, DOFPulse, DOFContactors)
		Zipper=1
		UnZipTimer.enabled=0
		ZipTimer.enabled=1
		LeftFlipper1.enabled=1 : RightFlipper1.enabled=1 : LeftFlipper.enabled=0 : RightFlipper.enabled=0
		FlipperLSh.visible = false
		FlipperRSh.visible = false
		FlipperLSh1.visible = true
		FlipperRSh1.visible = true
	End If
	TmushC.TransZ = 6
	me.timerenabled = 1
	AddScore (100)
	Light30.state=1
	End if
End Sub 

Sub TrigMushC_timer()
	TmushC.TransZ = -6
	me.timerenabled = 0
End Sub


Sub TrigMushR_Hit
	DOF 116, DOFPulse
	Dim MushRSound         
	MushRSound = Int(rnd*3)+1
	Select Case MushRSound
		Case 1: PlaySoundAt "flip_hit_1", TrigMushR
		Case 2: PlaySoundAt "flip_hit_2", TrigMushR
		Case 2: PlaySoundAt "flip_hit_3", TrigMushR
	End Select
	If tilt=false Then
	If Zipper=1 Then
		Playsound SoundFXDOF ("unzipflip", 119, DOFPulse, DOFContactors)
		Zipper=0
		ZipTimer.enabled=0
		UnZipTimer.enabled=1
		LeftFlipper1.enabled=0 : RightFlipper1.enabled=0 : LeftFlipper.enabled=1 : RightFlipper.enabled=1
		FlipperLSh.visible = true
		FlipperRSh.visible = true
		FlipperLSh1.visible = false
		FlipperRSh1.visible = false
	End If
	TmushR.TransZ = 6
	me.timerenabled = 1
	If Odin = 1 Then OdinKick.enabled=1 End If
	AddScore (100)
	Light29.state=1
	End If
End Sub

Sub TrigMushR_timer()
	TmushR.TransZ = -6
	me.timerenabled = 0
End Sub

Sub OdinKick_timer()
	Kicker1.Kick 90, 3
	DOF 112, DOFPulse
	If Light24.state=1 Then AddScore 1000 : Light24.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	If Light25.state=1 Then AddScore 2000 : Light25.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	If Light26.state=1 Then AddScore 3000 : Light26.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	If Light27.state=1 Then AddScore 4000 : Light27.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	If Light28.state=1 Then AddScore 5000 : Light28.state=1 : BlinkingSC.enabled=0 : BlinkingSC1.enabled=0: BlinkingSC2.enabled=0: BlinkingSC3.enabled=0: BlinkingSC4.enabled=0 End If
	OWBalls=OWBalls+1
	Odin=0
	OWDrain.enabled=1
	OdinKick.enabled=0
End Sub

Sub WotanKick_timer()
	DOF 113, DOFPulse
	Kicker2.Kick 90 , 3
	OWBalls=OWBalls+1
	Wotan=0
	OWDrain.enabled=1
	WotanKick.enabled=0
End Sub

'********* Zipper Flippers *********

Sub ZipTimer_timer
	FlipperL.x = FlipperL.x + 10 '335 
	FlipperR.x = FlipperR.x - 10 '550
	if FlipperL.x > 334 Then
		FlipperL.x = 335
		FlipperR.x = 550
		me.enabled=0
	end If
End Sub

Sub UnZipTimer_timer
	FlipperL.x = FlipperL.x - 10 '305 
	FlipperR.x = FlipperR.x + 10 '580
	if FlipperL.x < 306 Then
		FlipperL.x = 305
		FlipperR.x = 580
		me.enabled=0
	end If
End Sub

'***************************************

Sub Trigger4_Hit
	'PlaySound "fx_trigger"
	If Tilt=false Then 
	Kicker4.enabled=1 
	Light23.state=1
	End If

End Sub

Sub Trigger1_Hit
	'PlaySound "fx_trigger"
	Kicker4.enabled=0
	Light23.state=0

End Sub

Sub Trigger7_Hit
	'PlaySound "fx_trigger"
	Kicker4.enabled=0
	Light23.state=0

End Sub

Sub Trigger2_Hit
	'PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mDISC.MotorOn = 0
	stopsound "disc_noise"
	stopsound "disc_noise1"
	stopsound "Disc_Noise2"

End Sub

Sub Trigger3_Hit
	'PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mDISC.MotorOn = 1
	stopsound "disc_noise"
	Playsound "disc_noise" ,-1,0.005
End Sub

Sub Trigger5_Hit
	'PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mDISC.MotorOn = 0
	stopsound "disc_noise"
	stopsound "disc_noise1"
	stopsound "Disc_Noise2"
End Sub

Sub Trigger6_Hit
	'PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mDISC.MotorOn = 1
	stopsound "disc_noise1"
	Playsound "disc_noise1" ,-1,0.005
End Sub

Sub Trigger8_Hit
	'PlaySound "fx_trigger"
	If Tilt=false Then AddScore (10) End If
	mDISC.MotorOn = 1
	stopsound "Disc_Noise2"
	Playsound "disc_noise2" ,-1,0.005
End Sub

Sub Kicker4_Hit
	me.timerenabled=1
End Sub

Sub Kicker4_Timer()
	DOF 111, DOFPulse
	Strong = Int(Rnd*9)+21
	Kicker4.Kick 0, Strong
	Stantuffo.enabled=1
	AddScore (1000)
	me.timerenabled=0
End Sub

Sub Stantuffo_timer()
	Primitive83.TransY=-70
	StantuffoR.enabled=1
	Stantuffo.enabled=0
End Sub

Sub StantuffoR_timer()
	Primitive83.TransY=Primitive83.TransY+5
	If Primitive83.TransY=0 Then
	StantuffoR.enabled=0
	End If
End Sub

Sub Kicker4_UnHit
	'PlaySound "popper_ball",0,.75,0,0.25
	PlaySoundAt "popper_ball", Kicker4
End Sub


Sub Wall66_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber1.visible=0
	Rubber11.visible=1
	RubberTimer.enabled=1
End Sub

Sub RubberTimer_timer()
	Rubber1.visible=1
	Rubber11.visible=0
	RubberTimer.enabled=0
End Sub

Sub Wall67_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber2.visible=0
	Rubber13.visible=1
	RubberTimer1.enabled=1
End Sub

Sub RubberTimer1_timer()
	Rubber2.visible=1
	Rubber13.visible=0
	RubberTimer1.enabled=0
End Sub

Sub Wall68_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber10.visible=0
	Rubber26.visible=1
	RubberTimer2.enabled=1
End Sub

Sub RubberTimer2_timer()
	Rubber10.visible=1
	Rubber26.visible=0
	RubberTimer2.enabled=0
End Sub

Sub Wall69_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber14.visible=0
	Rubber20.visible=1
	RubberTimer3.enabled=1
End Sub

Sub RubberTimer3_timer()
	Rubber14.visible=1
	Rubber20.visible=0
	RubberTimer3.enabled=0
End Sub

Sub Wall70_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber12.visible=0
	Rubber27.visible=1
	RubberTimer4.enabled=1
End Sub

Sub RubberTimer4_timer()
	Rubber12.visible=1
	Rubber27.visible=0
	RubberTimer4.enabled=0
End Sub

Sub Wall71_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber15.visible=0
	Rubber28.visible=1
	RubberTimer5.enabled=1
End Sub

Sub RubberTimer5_timer()
	Rubber15.visible=1
	Rubber28.visible=0
	RubberTimer5.enabled=0
End Sub

Sub Wall72_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber9.visible=0
	Rubber29.visible=1
	RubberTimer6.enabled=1
End Sub

Sub RubberTimer6_timer()
	Rubber9.visible=1
	Rubber29.visible=0
	RubberTimer6.enabled=0
End Sub

Sub Wall73_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber5.visible=0
	Rubber30.visible=1
	RubberTimer7.enabled=1
End Sub

Sub RubberTimer7_timer()
	Rubber5.visible=1
	Rubber30.visible=0
	RubberTimer7.enabled=0
End Sub

Sub Wall74_Hit
	PlaySound "target" , 1,0.5
	If Tilt=false Then AddScore (10) End If
	Rubber4.visible=0
	Rubber31.visible=1
	RubberTimer8.enabled=1
End Sub

Sub RubberTimer8_timer()
	Rubber4.visible=1
	Rubber31.visible=0
	RubberTimer8.enabled=0
End Sub

'*****************************************

'********** Sling Shot Animations ***************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RandomSoundSlingshotRight sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -25
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	If Tilt = false Then
	Addscore (10)
	End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	RandomSoundSlingshotLeft sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -25
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	If Tilt = false Then
	Addscore (10)
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1

End Sub

'*******************************************

'********** Bumpers **********************************************

	
Sub Bumper1_Hit
	If Tilt = false Then
		RandomSoundBumperTop Bumper1
		If Light32.state = 0 Then 
			Addscore (10)
		End If
		If Light32.state = 1 Then 
			Addscore (100)
		End If
	End If
	
End Sub


Sub Bumper2_Hit
	If Tilt = false Then
        
		RandomSoundBumperMiddle Bumper2
		If Light33.state = 0 Then 
			Addscore (10)
		End If
		If Light33.state = 1 Then 
			Addscore (100)
		End If
	End If
	
End Sub


Sub Bumper3_Hit
	If Tilt = false Then
		RandomSoundBumperBottom Bumper3
		Addscore (100)
	End If
End Sub


'*****************************************************************************

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

'Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "Ramones" is the name of the table
'    Dim tmp
'    tmp = ball.x * 2 / Ramones.width-1
'    If tmp > 0 Then
'        Pan = Csng(tmp ^10)
'    Else
'        Pan = Csng(-((- tmp) ^10) )
'    End If
'End Function


'*****************************************
'			BALL SHADOW
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
        If BOT(b).X < Ramones.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/10) + ((BOT(b).X - (Ramones.Width/2))/13))
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/10) + ((BOT(b).X - (Ramones.Width/2))/13))
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



'********* Others Sounds *************

Sub Trigger38_Hit
	PlaySoundAt "metal_hop", Trigger38
	Trigger41.enabled=1
End Sub

Sub Trigger41_Hit
	'PlaySound "drop",0,0.5
	PlaySoundAt "drop", Trigger41
	Trigger41.enabled=0
End Sub

Sub Wall75_Hit
	'PlaySound "toc",0,0.5
	PlaySoundAt "toc", Wall75
	Wall75.enabled=0
End Sub

Sub Rubber32_Hit
	PlaySoundAt "flip_hit_1", Rubber32
	Rubber32.enabled=0
End Sub


Sub CallFlipOff_Timer()
	FlipOff
	CallFlipOff.enabled=0
End Sub


'Sub Plastics_Hit (idx)
'	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
'End Sub
'
'Sub Metals_Thin_Hit (idx)
'	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'End Sub
'
'Sub Rubbers_Hit(idx)
' 	dim finalspeed
'  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
' 	If finalspeed > 20 then 
'		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'	End if
'	If finalspeed >= 6 AND finalspeed <= 20 then
' 	RandomSoundRubber()
' 	End If
'End Sub
'
'Sub Posts_Hit(idx)
' 	dim finalspeed
'  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
' 	If finalspeed > 16 then 
'		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'	End if
'	If finalspeed >= 6 AND finalspeed <= 16 then
' 		RandomSoundRubber()
' 	End If
'End Sub
'
'Sub RandomSoundRubber()
'	Select Case Int(Rnd*3)+1
'		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'	End Select
'End Sub

Sub LeftFlipper_Collide(parm)
    'PlaySound "fx_rubber2", 0, parm / 10, -0.1, 0.15
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
    'PlaySound "fx_rubber2", 0, parm / 10, 0.1, 0.15
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

Sub LeftFlipper1_Collide(parm)
    'PlaySound "fx_rubber2", 0, parm / 10, -0.1, 0.15
	CheckLiveCatch Activeball, LeftFlipper1, LFCount01, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
    'PlaySound "fx_rubber2", 0, parm / 10, 0.1, 0.15
	CheckLiveCatch Activeball, RightFlipper1, RFCount01, parm
	RightFlipperCollide parm
End Sub

'*************************************


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
Dim HSScoreM,HSScore100k, HSScore10k, HSScoreK, HSScore100, HSScore10, HSScore1, HSScorex	'Define 6 different score values for each reel to use
HSArray = Array("Postit0","postit1","postit2","postit3","postit4","postit5","postit6","postit7","postit8","postit9","postitBL","postitCM","Tape")
Const hsFlashDelay = 4

' ***********************************************************
'  HiScore DISPLAY 
' ***********************************************************

Sub UpdatePostIt
	dim tempscore
	HSScorex = hisc
	TempScore = HSScorex
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
	Pscore6.image = HSArray(HSScoreM):If HSScorex<1000000 Then PScore6.image = HSArray(10)
	Pscore5.image = HSArray(HSScore100K):If HSScorex<100000 Then PScore5.image = HSArray(10)
	PScore4.image = HSArray(HSScore10K):If HSScorex<10000 Then PScore4.image = HSArray(10)
	PScore3.image = HSArray(HSScoreK):If HSScorex<1000 Then PScore3.image = HSArray(10)
	PScore2.image = HSArray(HSScore100):If HSScorex<100 Then PScore2.image = HSArray(10)
	PScore1.image = HSArray(HSScore10):If HSScorex<10 Then PScore1.image = HSArray(10)
	PScore0.image = HSArray(HSScore1):If HSScorex<1 Then PScore0.image = HSArray(10)
	if HSScorex<1000 then
		PComma.image = HSArray(10)
	else
		PComma.image = HSArray(11)
	end if
	if HSScorex<1000000 then
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

'******************************************************
'              NFozzy
'******************************************************


'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
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
        FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
        FlipperTricks LeftFlipper1, LFPress01, LFCount01, LFEndAngle01, LFState01
        FlipperTricks RightFlipper1, RFPress01, RFCount01, RFEndAngle01, RFState01
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
Const PI = 3.1415927

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

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, LFPress01, RFPress, RFPress01, LFCount, LFCount01, RFCount, RFCount01
dim LFState, LFState01, RFState, RFState01
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, RFEndAngle01, LFEndAngle, LFEndAngle01

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
LFEndAngle01 = LeftFlipper1.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle01 = RightFlipper1.endangle

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
RubbersD.addpoint 0, 0, 0.96        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

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
                RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
                coef = desiredcor / realcor 
                if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
                "actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
                if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
                
                aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
                if debugOn then TBPout.text = str
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

Sub RDampen_Timer()
Cor.Update
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
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

Dim tablewidth, tableheight : tablewidth = Ramones.width : tableheight = Ramones.height

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
	Debug.Print "Table Object" & tableobj.name
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
'		BALL ROLLING AND DROP SOUNDS
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
	Next
End Sub





