Option Explicit
Randomize
LoadCoreVBS

'      _            _    
'     | |          | |   
'   __| | __ _ _ __| | __
'  / _` |/ _` | '__| |/ /
' | (_| | (_| | |  |   < 
'  \__,_|\__,_|_|  |_|\_\
'                    _                         
'                   (_)                        
'         _ __  _ __ _ _ __   ___ ___  ___ ___ 
'        | '_ \| '__| | '_ \ / __/ _ \/ __/ __|
'        | |_) | |  | | | | | (_|  __/\__ \__ \
'        | .__/|_|  |_|_| |_|\___\___||___/___/ V1.3 final FUN/Pro
'        | |                                   
'        |_|              Original by Froggy Crew
'
'
' Original Idea and code dev. : 	Gaetan Joly [aka Emperor312]
' Artwork/lights/integration : 		Franck Hollinger [aka Mussinger]
' 3D Primitive, Princess molding : 	DCrosby (Thank you so much, you killed it!!!)
'
' Thanks to Oker for all his work on the MXleds DOF, it looks amazing man !
' Thanks to all the Beta-Testers specialy to Rik, Bertrand and Ludovic.
'****************************** (¯`·._.·(¯`·._.·  Changelog  ·._.·´¯)·._.·´¯) *******************************

'Changelog v1.3 FUN/PRO

'14/01/2021
'-Add a FUN and Pro Mod (select on startgame)
'-Add a Jukebox mod (Pull Plunger or launchball)
'-Add a DMD options (Size and Position)
'-Fun mod whith ramdom Missions 
'-PRO mod for progress Missions 
'-Correct some little bug


'Changelog v1.2

'08/01/2021
'-Correction of little bugs
'-Add New Highscore systeme with 3 highscores et Ingame initials entry.
'-Loop For attract DMD

 'Changelog V1.1

'06/01/2021
'-Loop All Musics Files -Some correction of DOF
'-Some Correction in tables lights and rules 
'-Bug correction 
'-Correction of little bug with callout
'
'****************************** (¯`·._.·(¯`·._.·  Music Credit  ·._.·´¯)·._.·´¯) *******************************
'Intro Music : It's A Small World (Dark Version) by Mathieu Verdier
'Playmusic : Furious Eating Music by Liddell Woodman; 
'RAPUNZEL : Alice - What Have You Done by within temptation 
'ARIEL : Poor Unfortunate Souls by Jonathan Young
'AURORA :  Sleeping Beauty by Dreamtale 
'BEAUTY : Beauty and the Beast by Caleb Hyles
'CINDERELLA : A Dream Is A Wish Your Heart Makes by Justin Muncy
'ELSA : Let it Go, by Leo Moracchioli (Frog Leap Studios)
'Multiball : SPEED/POWER/HEAVY/METAL instrumental, by Jose Hernandez Santis
': 
'
'****************************** (¯`·._.·(¯`·._.·  Player Options  ·._.·´¯)·._.·´¯) *******************************

Dim Voice : Voice=2				'Voice callouts: Choose the Voice gender 0=Male and 1=Female ans 2=Mix of both
Dim TDOF : Tdof=0 				'Round Targets: Activate DOF contactor for target 0=off 1=on (Default is 0)			
Dim Glevel : Glevel=0			'Choose Difficult Mod Fun/Pro 0=Fun:Ramdom missions And 1=Progressive Missions
'****************************** (¯`·._.·(¯`·._.·  Sounds Options  ·._.·´¯)·._.·´¯) *******************************

Dim Mlevel : Mlevel=0.4			'Music volume: Change sound music level 0=Off and 1= Max (Default is 0.4)
Dim Blevel : Blevel=0.1			'Drum on Bumper volume: Change Bumper drum level 0=off and 1=Max
Dim BGlevel : BGlevel=1 		'Backglass volume: Change BackGlass level sound 0=off and 1=max (Default is 1)
'****************************** (¯`·._.·(¯`·._.·  DMD Options  ·._.·´¯)·._.·´¯) ************************************


DMDPosition = "False"                               ' Use Manual DMD Position, True / False
DMDPosX = 2168                                   ' Position in Decimal
DMDPosY = 810                                     ' Position in Decimal

DMDSize = "False"                                     ' Use Manual DMD Size, True / False
DMDWidth = 512                                  ' Width in Decimal
DMDHeight = 128                                   ' Height in Decimal 


'*******************************************************************************************************************

Dim GifTime1 : GifTime1=2400
Dim GifTime2 : GifTime2=5500



Sub LoadCoreVBS
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Goto 0
End Sub

Dim Controller
Set Controller = CreateObject("B2S.Server")
Controller.B2SName = "DarkPrincess"
Controller.Run ()
Dim Ball
Dim Balls
Dim Score
Dim ScoreToAdd
Dim InProgress: Inprogress=False
Dim Credits: Credits=0
Dim light
ball=0
balls=3
score=0
Ballinpf=0
Dim Ballinpf
Dim M1Inprogress: M1Inprogress=False
Dim M2Inprogress: M2Inprogress=False
Dim M3Inprogress: M3Inprogress=False
Dim M4Inprogress: M4Inprogress=False
Dim M5Inprogress: M5Inprogress=False
Dim M6Inprogress: M6Inprogress=False
Dim M7Inprogress: M7Inprogress=False
Dim M8Inprogress: M8Inprogress=False
Dim M9Inprogress: M9Inprogress=False
Dim M10Inprogress: M10Inprogress=False
Dim M11Inprogress: M11Inprogress=False
Dim M12Inprogress: M12Inprogress=False
Dim MissionInProgress: MissionInProgress=False
Dim MULTILOCK: MULTILOCK=False
Dim Kickbackinprogress: kickbackinprogress=false
Dim MULTIELSA: MULTIELSA=False
Dim Shad : shad=0
Dim FreePlay : FreePlay=1 
Dim Extrainprogress : Extrainprogress=False
Const cGameName = "DarkPrincess"
Const HSFileName= "DarkPrincess.txt"
Dim HighScore(4)
Dim HighscoreName(4)
Const TableName = "DarkPrincess"
Dim Moveit : Moveit=1
Const constMaxMultiplier=5
Dim missionDouble : missionDouble=False			
'First, try to load the Controller.vbs (DOF), which helps controlling additional hardware like lights, gears, knockers, bells and chimes (to increase realism)
'This table uses DOF via the 'SoundFX' calls that are inserted in some of the PlaySound commands, which will then fire an additional event, instead of just playing a sample/sound effect
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

'If using Visual PinMAME (VPM), place the ROM/game name in the constant below,
'both for VPM, and DOF to load the right DOF config from the Configtool, whether it's a VPM or an Original table



'*****************************MUSIC******************************
'****************************************************************
'****************************************************************
Dim IntroMusicIP : IntroMusicIP=False
'*************************************
Sub DPPlay_MusicDone
Playmusic "DPPlay.mp3",Mlevel
playTime.Enabled=True
End Sub

Sub PlayTime_Timer
If MissionInProgress=False Then
PlayTime.Enabled=True
DPPlay_MusicDone
Else
PlayTime.Enabled=False
End If
End Sub
'*************************************
Sub IntroTime_Timer
If IntroMusicIP=True Then 
IntroTime.Enabled=True
DPintro_MusicDone
End If
If IntromusicIp=False Then
IntroTime.Enabled=False
End If
End Sub

Sub DPintro_MusicDone
IntroTime.Enabled=True
Playmusic "DPIntro.mp3"
End Sub

'**************************************

Sub DPMulti_MusicDone
Playmusic "DPmulti.mp3",Mlevel
MultiBTime.Enabled=True
End Sub

Sub MultiBTime_Timer
If BallinPf>1 Then
MultiBTime.Enabled=True
DPMulti_MusicDone
Else
MultiBTime.Enabled=False
End if 
End Sub
'**************************************
Sub DPAlice_MusicDone
Playmusic "DPALICE.mp3",Mlevel
AliceTime.Enabled=True
End Sub

Sub AliceTime_Timer
If M1Inprogress=True Or M7Inprogress=True Then
AliceTime.Enabled=True
DPAlice_MusicDone
Else
AliceTime.Enabled=False
End If
End Sub
'**************************************
Sub DPBeauty_MusicDone
PlayMusic "DPBEAUTY.mp3",Mlevel
BeautyTime.Enabled=True
End Sub

Sub BeautyTime_Timer
If M3Inprogress=True  Or M9Inprogress=True Then
BeautyTime.Enabled=True
DPBeauty_MusicDone
Else
BeautyTime.Enabled=False
End If
End Sub
'**************************************
Sub DPAriel_MusicDone
Playmusic "DPARIEL.mp3",Mlevel
ArielTime.Enabled=True
End Sub

Sub ArielTime_Timer
If M2Inprogress=True  Or M8Inprogress=True Then
ArielTime.Enabled=True
DPAriel_MusicDone
Else
ArielTime.Enabled=False
End If
End Sub
'**************************************
Sub DPAurore_MusicDone
Playmusic "DPCinderella.mp3",Mlevel
AuroreTime.Enabled=True
End Sub

Sub AuroreTime_Timer
If M5Inprogress=True  Or M11Inprogress=True Then
AuroreTime.Enabled=True
DPAurore_MusicDone
Else
AuroreTime.Enabled=False
End If
End Sub
'**************************************
Sub DPSnow_MusicDone
Playmusic "DPSNOWWHITHE.mp3",Mlevel
SnowTime.Enabled=True
End Sub

Sub SnowTime_Timer
If M4Inprogress=True Or M10Inprogress=True Then
SnowTime.Enabled=True
DPSnow_MusicDone
Else
SnowTime.Enabled=False
End If
End Sub
'**************************************
Sub DPElsa_MusicDone
Playmusic "DPELSA.mp3",Mlevel
WizzTime.Enabled=True
End Sub

Sub WizzTime_Timer
If M6Inprogress=True Then
WizzTime.Enabled=True
DPElsa_MusicDone
Else
WizzTime.Enabled=False
End If
End Sub

'************************JUKEBOX**********************************
Dim Jukebox : jukebox=0
Dim NextSong

Sub Jukeplay()
Endmusic

Select Case NextSong
Case 1 : 
juketime.interval=235000
Juketime.Enabled=True
Playmusic "DPALICE.mp3",Mlevel
 Case 2 : 

PlayMusic "DPBEAUTY.mp3",Mlevel
juketime.interval=321000
Juketime.Enabled=True
Case 3 :
juketime.interval=196000
Juketime.Enabled=True
Playmusic "DPARIEL.mp3",Mlevel
Case 4 :
juketime.interval=311000
Juketime.Enabled=True
Playmusic "DPCinderella.mp3",Mlevel
Case 5 :
juketime.interval=270000
Juketime.Enabled=True
Playmusic "DPSNOWWHITHE.mp3",Mlevel
Case 6 :
juketime.interval=242000
Juketime.Enabled=True
Playmusic "DPELSA.mp3",Mlevel
Case 7 :
juketime.interval=141000
Juketime.Enabled=True
Playmusic "DPmulti.mp3",Mlevel
Case 8 :
juketime.interval=377000
Juketime.Enabled=True
Playmusic "DPPlay.mp3",Mlevel
NextSong=0

End Select

End Sub
Dim nextname

Sub jukename()
Select case nextname
Case 1 : 
UltraDMD.DisplayScene00 "DPAlice.png", "", 15, "", 15, 14, 100, 14
Case 2 :
UltraDMD.DisplayScene00 "DPBEAUTY.png", "", 15, "", 15, 14, 100, 14
Case 3 : 
UltraDMD.DisplayScene00 "DPARIEL.png", "", 15, "", 15, 14, 100, 14
Case 4 :
UltraDMD.DisplayScene00 "dpcinderella.png", "", 15, "", 15, 14, 100, 14
Case 5 : 
UltraDMD.DisplayScene00 "DPSNOW.png", "", 15, " ", 15, 14,100, 14
Case 6 :
UltraDMD.DisplayScene00 "dpelsa.png", "", 15, "", 15, 14, 100, 14
Case 7 :
UltraDMD.DisplayScene00 "DPMULTI.png", "", 15, "", 15, 14, 100, 14
Case 8 :
UltraDMD.DisplayScene00 "DPPLAY.png", "", 15, "", 15, 14, 100, 14
nextName=0
End Select
End Sub

Sub JukeTime_Timer
JukeTime.Enabled=False
EndMusic
jukename
Jukeplay
NextSong=Nextsong+1
NextName=nextname+1
End Sub 






'*****************************************************************
'*****************************************************************
'*****************************************************************
'						MISSION RND
'*****************************************************************
'*****************************************************************
'*****************************************************************


Sub RamdomMission
Dim x
x = INT(5 * RND(1) )
Select Case x
		
	Case 0:If LM22.State=1 or kick<2 Then
			RamdomMission
			Else
			Mission1
			End If
	Case 1:If LM32.State=1 Then
			RamdomMission
			Else
			Mission2
			End If
	Case 2:If LM42.State=1 Then
			RamdomMission
			Else
			Mission3
			End If
	Case 3:If LM002.State=1 Then
			RamdomMission
			Else
			Mission4
			End If
	Case 4:If LM62.State=1 Then
			RamdomMission
			Else
			Mission5
			End If
End Select
End Sub

Sub RamdomMission2
Dim x
x = INT(5 * RND(1) )
Select Case x
		
	Case 0:If LM22.State=1  Then
			RamdomMission2
			Else
			Mission7
			End If
	Case 1:If LM32.State=1 Then
			RamdomMission2
			Else
			Mission8
			End If
	Case 2:If LM42.State=1 Then
			RamdomMission2
			Else
			Mission9
			End If
	Case 3:If LM002.State=1 Then
			RamdomMission2
			Else
			Mission10
			End If
	Case 4:If LM62.State=1 Then
			RamdomMission2
			Else
			Mission11
			End If
End Select
End Sub

Sub MissionSelect_Timer
MissionSelect.Enabled=False
If missionDouble=False then
RamdomMission
End If
If MissionDouble=True Then
RamdomMission2
End If

End Sub

Sub ImageMission
If LEXTRA1.State=0 then
DMDFLush
UltraDMD.DisplayScene00 "M1.gif", "", 0, "", 0, 14, 2000, 15
End If
MissionSelect.Enabled=True
End Sub

Sub ImageMission2
If LEXTRA1.State=0 then
DMDFLush
UltraDMD.DisplayScene00 "M1.gif", "", 0, "", 0, 14, 2000, 15
End If
MissionSelect.Enabled=True
End Sub









'*******************************DOF*******************************
Sub DOF(dofevent, dofstate) 
	If dofstate = 2 Then
		Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
	Else
		Controller.B2SSetData dofevent, dofstate
	End If
End Sub

'***************************Base TABLE****************************

Sub Table1_Init()
		Randomize
		StartLightSeq
		LoadUltraDMD
		Balls=3
		Score=0
		ball=0
		lock1=0
		Ballinpf=0
		SS=0
		DPintro_MusicDone
		IntroMusicIP=true
		loadhs
		Mextra=0
		RunAllScenes
		SoundStart.Enabled=True
		Controller.B2SSetData 399, 1     ' DOF MX Attract Mode Start
End Sub

Sub SoundStart_Timer
If Voice =1 Or Voice=2 Then playsound "welcomeF",0,BGlevel End If
If Voice =0 Then playsound "welcomeM",0,BGlevel End If
SoundStart.Enabled=False
End Sub

Sub NewGame
		InProgress=True
		Controller.B2SStopAnimation "Attract"
		TimerFlash.Enabled = False
		DrainTimer.Interval=3000
		DrainTimer.Enabled = True
		Lanelight1.State=2
		locklight1.State=2
		LT001.State=1
		Lstart.State=1
		Jacklight.State=1
		LEXTRA1.State=0
		Resetlights
		BonusMult=1
		balls=3
		Score=0
		Ball=0
		Ballinpf=0
		bsa=0
		SS=0
		lock1=0
		kick=0
		Rampb=0
		M4R=0
		TB=0
		M=0
		B1=0
		B2=0
		x2=0
		x3=0
		x4=0
		x5=0
		x6=0
		m7=0
		m8=0
		m9=0
		m10=0
		m11=0
		Extraball=0
		DMDScore
		Mextra=0
		M1Inprogress=False
		M2Inprogress=False
		M3Inprogress=False
		M4Inprogress=False
		M5Inprogress=False
		M6Inprogress=False
		MissionInProgress=False
		missionDouble=False
		loadhs
		IntroMusicIP=False
		EndMusic
		DPPlay_Musicdone
		TimerDmdIntro.Enabled=False
		DMDFLush

		DMDTime.Enabled=True
			Controller.B2SSetData 399, 0	      ' DOF MX Attract Mode Stop
			Controller.B2SSetData 334, 1	      ' DOF MX Start
			Controller.B2SSetData 334, 0	      ' DOF MX Start
		
End Sub

Sub GameOver
		InProgress=False
		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart
		Controller.B2SSetData 101, 0
		Controller.B2SSetData 102, 0
		LightSeqAttract_PlayDone
		IntroMusicIP=True
		EndMusic
		DPintro_MusicDone
		If Voice =1 Then playsound "GameoverF",0,BGlevel End If
		If Voice =0 Or Voice=2 Then playsound "GameoverM",0,BGlevel End If
		UltraDMD.CancelRendering
		UltraDMD.DisplayScene00 "gameover.gif", "", 0, "", 0, 14, GifTime1, 14
		UltraDMD.DisplayScene00 "blank.png", "Score", 0, "" & Score, 0, 14, 5000, 14
		Kickbackinprogress=False
		ResetLock
		RunAllScenes
		TimerDmdIntro.Enabled=True
End Sub

Sub Table1_Exit
	If InProgress=False Then
	savehs
	End If
	Controller.Stop
	End Sub





'*****************************Score et Bonus*******************************






Sub AddScore(ScoreToAdd)
		Score=Score+ScoreToAdd
		DMDScore 
End Sub

Sub DMDTime_Timer
if Inprogress=True then
DMDScore
DMDtime.Enabled=True
Else
Dmdtime.Enabled=False
End If
End Sub


Dim bonus

Sub Addbonus(bonustoadd)
		Bonus=Bonus+bonustoadd
End Sub


Sub EndOfBallBonus()
	If BonusMult=1 Then
		UltraDMD.DisplayScene00 "blank.png", "BONUS" , 15, Bonus & " X1", -1, 0, 1000, 1
		AddScore (bonus)
	End If
	If BonusMult=2 Then
		UltraDMD.DisplayScene00 "blank.png", "BONUS" , 15, bonus &" X2", -1, 0, 1000, 1
		Addscore (bonus)
		Addscore (bonus)
	End If
	If BonusMult=3 Then
		UltraDMD.DisplayScene00 "blank.png", "BONUS" , 15, bonus &" X3", -1, 0, 1000, 1
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
	End If
	If BonusMult=4 Then
		UltraDMD.DisplayScene00 "blank.png", "BONUS" , 15, bonus &" X4", -1, 0, 1000, 1
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
	end If
	If bonusMult=5 Then
		UltraDMD.DisplayScene00 "blank.png", "BONUS" , 15, bonus &" X5", -1, 0, 1000, 1
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
	End If
	If BonusMult=6 Then
		UltraDMD.DisplayScene00 "blank.png", "BONUS" , 15, bonus &" X6", -1, 0, 1000, 1
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
		Addscore (bonus)
	End If
		bonus=0
		BonusMult=1
		Lx2.State=0
		Lx3.State=0
		Lx4.State=0
		LX5.State=0
		LX6.State=0
		kb=0
		x2=0
		x3=0
		x4=0
		x5=0
		x6=0
		Lanelight1.State=2
End Sub

Dim x2,x3, x4, x5, x6

	
Sub CheckXbonus()
	If BonusMult=1 Then
	end If
	If BonusMult=2 And x2=0 Then
	Lx2.State=1
	x2=1
       UltraDMD.DisplayScene00 "bonusx2.gif", "", 0, "", 0, 14, GifTime1, 14
	PlaySound "Drum1",0,BGlevel
			Controller.B2SSetData 350, 1	      ' DOF MX Start
			Controller.B2SSetData 350, 0	      ' DOF MX Start
	DMDScore
	End If
	If BonusMult=3 And x3=0 Then
     UltraDMD.DisplayScene00 "bonusx3.gif", "", 0, "", 0, 14, GifTime1, 14
	PlaySound "Drum1",0,BGlevel
			Controller.B2SSetData 351, 1	      ' DOF MX Start
			Controller.B2SSetData 351, 0	      ' DOF MX Start
	DMDScore
	Lx3.State=1
	x3=1
	End If
	If BonusMult=4 And x4=0 Then
	LX4.State=1
     UltraDMD.DisplayScene00 "bonusx4.gif", "", 0, "", 0, 14, GifTime1, 14
	PlaySound "Drum1",0,BGlevel
			Controller.B2SSetData 352, 1	      ' DOF MX Start
			Controller.B2SSetData 352, 0	      ' DOF MX Start
	DMDScore
	x4=1
	End If
	If BonusMult=5 And x5=0 Then
     UltraDMD.DisplayScene00 "bonusx5.gif", "", 0, "", 0, 14, GifTime1, 14
	PlaySound "Drum1",0,BGlevel
			Controller.B2SSetData 353, 1	      ' DOF MX Start
			Controller.B2SSetData 353, 0	      ' DOF MX Start
	DMDScore
	LX5.State=1
	x5=1
	End If
	If BonusMult=6 And x6=0 Then
     UltraDMD.DisplayScene00 "bonusx6.gif", "", 0, "", 0, 14, GifTime1, 14
	PlaySound "Drum1",0,BGlevel
			Controller.B2SSetData 354, 1	      ' DOF MX Start
			Controller.B2SSetData 354, 0	      ' DOF MX Start
	DMDScore
	LX6.State=1
	BonusMult=6
	x6=1
	End If
End Sub

'********Resetlight****************
 Sub Resetlights
		TLight001.State=0
		TLight002.State=0
		TLight003.State=0
		TLight004.State=0
		TLight005.State=0
		TLight006.State=0
		TLight007.State=0
		TLight008.State=0
		Lramp1.State=0
		LRamp2.State=0
		LRamp3.State=0
		Lramp4.State=0
		LRamp5.State=0
		LRamp6.State=0
		LT1.State=0
		Lt2.State=0
		Lt3.State=0
		LT4.State=0
		LT5.State=0
		LT6.State=0
		LT8.State=0
		LT9.State=0
		LT10.State=0
		LT11.State=0
		lanelight2.State=0
		Lanelight3.State=0
		Klight.State=0
		Klight001.State=0
		Klight002.State=0
		locklight2.State=0
		locklight3.State=0
		Jacklight.State=0
		DoubleJacklight.State=0
		Superjacklight.State=0
		LJE1.State=0
		LJE2.State=0
		LJE3.State=0
		LM32.State=0
		LM22.State=0
		LM42.State=0
		LM002.State=0
		LM62.State=0
		ExtraBLight.State=0
End Sub

Sub ResetLock
Targetlocl1.IsDropped=True
Targetlock2.IsDropped=True
Targetlock3.IsDropped=True
Balllock1.kick 100,5
Balllock2.Kick 100,25
Balllock2.Enabled=False
KickerLock.Enabled=False
ResetTimer.Enabled=True
End Sub

Sub ResetTimer_timer
ResetTimer.Enabled=False
Targetlocl1.IsDropped=False
Targetlock2.IsDropped=False
Targetlock3.IsDropped=False
KickerLock.Enabled=True
End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  High Scores
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	' load em up


Dim hschecker:hschecker = 0

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 6500000 End If

    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "DRK" End If

    x = LoadValue(cGameName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 4500000 End If

    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "PRC" End If

    x = LoadValue(cGameName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If

    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "FRC" End If

    x = LoadValue(cGameName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If

    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "EVI" End If

    'x = LoadValue(cGameName, "TotalGamesPlayed")
    'If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    'SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub ResetHS()
    SaveValue cGameName, "HighScore1", 6500000
    SaveValue cGameName, "HighScore1Name", "DRK"
    SaveValue cGameName, "HighScore2", 4500000
    SaveValue cGameName, "HighScore2Name", "PRC"
    SaveValue cGameName, "HighScore3", 1000000
    SaveValue cGameName, "HighScore3Name", "FRC"
    SaveValue cGameName, "HighScore4", 500000
    SaveValue cGameName, "HighScore4Name", "SUP"
	Loadhs
	dmdflush
	DMD2 "","HIGH SCORES", "ERASED", 3000, UltraDmd_eNone , ""
	'playsound "SFX_Commit",1,nFxVolume
	'TimerDmdIntro.enabled = False
	'TimerDmdIntro.enabled = True
	'dmdintroloop
	Table1_Init
End Sub

Dim totalgamesPlayed
Sub Savegp
	SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
	vpmtimer.addtimer 1000, "Loadhs'"
End Sub


' Initials

Dim hsbModeActive:hsbModeActive = False
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    Dim tmp,a
    tmp = Score:a=1

    

	

    If tmp> HighScore(1) Then 'add 1 credit for beating the highscore
        'Playsound "bonus"
	'DMD2 "","PLAYER "+cstr(a), "GOT HIGHSCORE", 5000, UltraDmd_eNone , ""
    End If

    If tmp> HighScore(3) Then
		'PlaySound "PFFX_Word_YouGotAHighScore",1,nFxVolume
        'vpmtimer.addtimer 2000, "PlaySound ""SFX_Speech_Initials"" '"
        HighScore(3) = tmp
	Playsound "bonus"
	DMD2 "","PLAYER "+cstr(a), "GOT HIGHSCORE", 5000, UltraDmd_eNone , ""
        'enter player's name
	HSEntry.Enabled=True
    Else
	HSGAME.Enabled=True
    End If
End Sub

Sub HSEntry_Timer
HighScoreEntryInit()
HSEntry.Enabled=False
End Sub

Sub HSGAME_Timer
GameOver
HSGAME.Enabled=False
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    hsLetterFlash = 0

    hsEnteredDigits(0) = "A"
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789" ' < is used to delete the last letter
    hsCurrentLetter = 1
    DMDFlush
    DMDId "hsc", "blank.png", "ENTER YOUR NAME:", " ", 999999
    HighScoreDisplayName()
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
		'DOF 182, DOFPulse 'left to right
        'Playsound "SFX_Electra1",1,nFxVolume
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
		'DOF 181, DOFPulse 'right to Left
        'Playsound "SFX_Faulty",1,nFxVolume
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey OR keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
           ' playsound "SFX_Commit",1,nFxVolume
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
            'playsound "SFX_Commit"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub

Sub HighScoreDisplayName()
    Dim i, TempStr

    TempStr = " >"
    if(hsCurrentDigit> 0) then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
    DMDMod "hsc", "ENTER YOUR NAME:", Mid(TempStr, 2, 5), 999999
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False
    'PlaySound "SFX_Commit",1,nFxVolume
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
	savehs
    DMDFlush
    HSGAME.Enabled=True
	Controller.B2SSetData 115, 1 'Knocker
	Controller.B2SSetData 115, 0
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) <HighScore(j + 1) Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub


const UltraDmd_eNone = 0
const UltraDMD_eScrollLeft = 1
const UltraDMD_eScrollRight = 2
const UltraDMD_eScrollUp = 3
const UltraDMD_eScrollDown = 4
const UltraDMD_eZoom = 5
const UltraDMD_eFade = 6

Const UltraDMD_VideoMode_Stretch=0, UltraDMD_VideoMode_Top = 1, UltraDMD_VideoMode_Middle = 2, UltraDMD_VideoMode_Bottom = 3
Const UltraDMD_Animation_FadeIn = 0, UltraDMD_Animation_FadeOut = 1, UltraDMD_Animation_ZoomIn = 2, UltraDMD_Animation_ZoomOut = 3
Const UltraDMD_Animation_ScrollOffLeft = 4, UltraDMD_Animation_ScrollOffRight = 5, UltraDMD_Animation_ScrollOnLeft = 6, UltraDMD_Animation_ScrollOnRight = 7,UltraDMD_Animation_ScrollOffUp = 8,UltraDMD_Animation_ScrollOffDown = 9,UltraDMD_Animation_ScrollOnUp = 10,UltraDMD_Animation_ScrollOnDown = 11,UltraDMD_Animation_None = 14


Sub DMDFLush
	if DMDon=1 Then
		UltraDMD.CancelRendering
	end if
End Sub
Dim DMDon : DMDon=1
Sub DMDId(id, background, toptext, bottomtext, duration)
If DMDon=1 then
		UltraDMD.DisplayScene00ExwithID id, False, background, toptext, 15, 0, bottomtext, 15, 0, 14, duration, 14
	end if
End Sub

Sub DMDMod(id, toptext, bottomtext, duration)
If DMDon=1 then
UltraDMD.ModifyScene00Ex id, toptext, bottomtext, duration
End If
End Sub


Sub DMD2(background, toptext, bottomtext, duration, animation, DmdSound)

	if DMDon=1then
		if background ="" or len(background) <3 then background="blank.png"
		if DmdSound <> "" and len(dmdsound) > 2 Then
			PlaySound DmdSound , 1, nTableSpeechVolume
		end if
		select case animation
			case UltraDmd_eNone
				UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, UltraDMD_Animation_None , duration, UltraDMD_Animation_None 
			case UltraDMD_eScrollLeft
				UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, UltraDMD_Animation_ScrollOnLeft , duration, UltraDMD_Animation_ScrollOffLeft  
			case UltraDMD_eScrollRight
				UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, UltraDMD_Animation_ScrollOnRight , duration, UltraDMD_Animation_ScrollOffRight 
			case UltraDMD_eScrollUp
				UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, UltraDMD_Animation_ScrollOnUp , duration, UltraDMD_Animation_ScrollOffUp 
			case UltraDMD_eScrollDown
				UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, UltraDMD_Animation_ScrollOnDown , duration, UltraDMD_Animation_ScrollOffDown   
			case UltraDMD_eZoom
				UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, UltraDMD_Animation_ZoomIn  , duration, UltraDMD_Animation_ZoomOut  
			case UltraDMD_eFade
				UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, UltraDMD_Animation_FadeIn   , duration, UltraDMD_Animation_FadeOut 
			case Else
				UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, UltraDMD_Animation_None , duration, UltraDMD_Animation_None 
		end select
		UltraDMDTimer.Enabled = 1 'to show the score after the animation/message'
	end if
End Sub
Dim Mextra	
'**********DRAIN**********

Sub Drain1_Hit()
     Drain1.DestroyBall
     Ballinpf=ballinpf -1
	PlaySound "Drain"
		If BallsaveLight.State=1 And Ballinpf=0 And bsa=0 Then
			bsa=bsa+1
			Kicker1.CreateBall
			Kicker1.Kick 90,10
			Plungertime.Enabled=True
If Voice =1 Or Voice=2 Then playsound "SaveF",0,BGlevel End If
If Voice =0 Then playsound "SaveM",0,BGlevel End If
			Controller.B2SSetData 104, 1
			Controller.B2SSetData 104, 0
			Controller.B2SSetData 301, 1     ' DOF MX Ball Save Start
			Controller.B2SSetData 301, 0     ' DOF MX Ball Save Start
			Ballinpf=Ballinpf +1
			BallsaveLight.State=0
        UltraDMD.DisplayScene00 "Ballsave.gif", "", 0, "", 0, 14, GifTime1, 14
			LightSeqAttract.StopPlay
			Flashevent3
		End If
		If Tballsave2.Enabled=True And BallsaveLight.State=1 Then
If Voice =1 Then playsound "SaveF",0,BGlevel End If
If Voice =0 Or Voice=2  Then playsound "SaveM",0,BGlevel End If
			Kicker1.CreateBall
			Kicker1.Kick 90,10
			Plungertime.Enabled=True

			Controller.B2SSetData 104, 1
			Controller.B2SSetData 104, 0
			Ballinpf=Ballinpf +1
		End If
		
		If extraball=1 and Ballinpf=0 And Mextra=<2 Then
			Kicker1.Createball
			Kicker1.kick 90,7
Extrainprogress=True
			Controller.B2SSetData 104, 1
			Controller.B2SSetData 104, 0
			extraball=extraball-1
			Ballinpf=ballinpf+1
If Voice =1 Or Voice=2 Then playsound "ShootagainF",0,BGlevel End If
If Voice =0 Then playsound "ShootagainM",0,BGlevel End If
UltraDMD.DisplayScene00 "ShootAgain.gif", "", 0, "", 0, 14, GifTime1, 14
			ExtraBLight.State=0
		End If
		If extraball=2 and Ballinpf=0 And mextra=<2 Then
			Kicker1.Createball
			Kicker1.kick 90,7
			Controller.B2SSetData 104, 1
			Controller.B2SSetData 104, 0
			extraball=extraball-1
				Ballinpf=ballinpf+1
			Extrainprogress=True
If Voice =1 Or Voice=2 Then playsound "ShootagainF",0,BGlevel End If
If Voice =0 Then playsound "ShootagainM",0,BGlevel End If
UltraDMD.DisplayScene00 "ShootAgain.gif", "", 0, "", 0, 14, GifTime1, 14
		End If
			
			If Ball < Balls And Ballinpf=0 And TBallsave.Enabled=False  And extraball=0 Then   ' Is ball played less than total Balls or is the Game Over
			DrainTimer.Interval=3000
			DrainTimer.Enabled = True ' If not, then let's play another ball, but let's set the timer for a delay
			SS=0 
			Multijacklight.State=0
			Lanelight1.State=1
			lanelight2.State=0
			Lanelight3.state=0
			Klight.State=0
			Controller.B2SSetData 302, 1     ' DOF MX  Drain
			Controller.B2SSetData 302, 0     ' DOF MX  Drain
			EndOfBallBonus
			bsa=0
			Mextra=0
			Kickbackinprogress=False
			Extrainprogress=False
		End If
	
		If ball=Balls And BallsaveLight.State=0 And Ballinpf=0 And Extraball=0 Then
			Controller.B2SStartAnimation "Attract"
			EndOfBallBonus
        
			Controller.B2SSetData 109, 1
			Controller.B2SSetData 109, 0
			Controller.B2SSetData 107, 1
			Controller.B2SSetData 107, 0
			Controller.B2SSetData 132, 1
			Controller.B2SSetData 132, 0
			Controller.B2SSetData 323, 1     ' DOF MX Game Over
			Controller.B2SSetData 323, 0     ' DOF MX Game Over
			Controller.B2SSetData 399, 1     ' DOF MX Attract Mode
			Controller.B2SSetData 390, 0     ' DOF MX Reset Princess Mode
			Controller.B2SSetData 391, 0     ' DOF MX Reset Princess Mode
			Controller.B2SSetData 392, 0     ' DOF MX Reset Princess Mode
			Controller.B2SSetData 393, 0     ' DOF MX Reset Princess Mode
			Controller.B2SSetData 394, 0     ' DOF MX Reset Princess Mode
			Controller.B2SSetData 395, 0     ' DOF MX Reset Princess Mode
			Extrainprogress=False
		Mextra=0
		Checkhighscore
		End If

		If Ballinpf=1 And Multilock=True And MissionInProgress=False Then
		MULTILOCK=False
		ResetTargets
		Endmusic
		DPPlay_musicDone
		locklight1.State=2
		locklight2.State=0
		locklight3.State=0
		Multijacklight.State=0
		lock1=0
		End If
		
		if Ballinpf=1 And Multilock=True And MissionInProgress=True Then
		MULTILOCK=False
		ResetTargets
		locklight1.State=2
		locklight2.State=0
		locklight3.State=0
		Multijacklight.State=0
		lock1=0
		End If
		
		
End Sub

Sub StartTimer1_Timer
StartTimer1.Enabled=False
NewGame
End Sub

Sub TimeHS_Timer
TimeHS.Enabled=False
GameOver
savehs
End Sub

Sub Draintimer_Timer()
			DrainTimer.Enabled = False    ' When timer runs out we will do the stuff below
			Kicker1.CreateBall
			Kicker1.Kick 90,10
			PlaySound "ballrelease"
			Ball = Ball + 1
		Ballinpf=ballinpf+1
			DMDScore
			LightSeqAttract.StopPlay
			Controller.B2SSetData 104, 1
			Controller.B2SSetData 104, 0
			Controller.B2SSetData 300, 1     ' DOF MX Ball is Ready to Shoot
End Sub
'************************************
'************************************
'******************************BALLCONTROL***********************
Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys
Dim ChooseP :ChooseP=False
Dim ChooseMod : ChooseMod=False
Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
		If Inprogress=True Then
		Plunger2.PullBack
		PlaySound "plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
		End IF
	If Inprogress=False And Jukebox=0 Then
DMDFLush
TimerDmdIntro.Enabled=False		
nextSong=NextSong+1
NextName=nextname+1
jukeplay
JukeName

	Jukebox=1
	End If
	
	End If


	If keycode = LeftFlipperKey Then
	If Inprogress=False And ChooseMod=True then 
		Glevel=0
PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		StartTimer1.Enabled=True
		ChooseMod=False
		UltraDMD.CancelRendering
		DMD2 "blank.png",  "","FUN  Select"  , 500 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "",""  , 50 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "","Fun  Select"  , 500 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "",""  , 50 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "","FUN  Select"  , 500 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "",""  , 50 , UltraDmd_eNone , ""
		End If

		if Inprogress=true Then
		Controller.B2SSetData 101, 1

		LeftFlipper.RotateToEnd
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		RotateLaneLightsLeft
	End If
		if Jukebox=1 Then
		NextSong=Nextsong-1
		NextName=nextname-1
		Jukeplay
		JukeName
	End If
		
	End If

	If keycode = RightFlipperKey Then
		If Inprogress=False And ChooseMod=True then 
		Glevel=1
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		StartTimer1.Enabled=True
		ChooseMod=False
		UltraDMD.CancelRendering
		DMD2 "blank.png",  "","Pro  Select"  , 500 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "",""  , 50 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "","Pro  Select"  , 500 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "",""  , 50 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "","Pro  Select"  , 500 , UltraDmd_eNone , ""
		DMD2 "blank.png",  "",""  , 50 , UltraDmd_eNone , ""
		End If
	if Inprogress=true Then	
		Controller.B2SSetData 102, 1

		RightFlipper.RotateToEnd
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		RotateLaneLightsRight
	End If
		if Jukebox=1 Then
		NextSong=Nextsong+1
		NextName=nextname+1
		Jukeplay
		JukeName
	End If
	End IF
	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
	
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If
	if keycode = 65 then Call ResetHS 'F7 reset High Scores
    If KeyCode = RightMagnaSave Then 
	DMD2 "blank.png",  "1 - " + HighScoreName(0),HighScore(0)  , 3000 , UltraDmd_eNone , ""
	DMD2 "blank.png",  "2 - " + HighScoreName(1),HighScore(1)  , 3000 , UltraDmd_eNone , ""
	DMD2 "blank.png",  "3 - " + HighScoreName(2),HighScore(2)  , 3000 , UltraDmd_eNone , ""
	DMD2 "blank.png",  "4 - " + HighScoreName(3),HighScore(3)  , 3000 , UltraDmd_eNone , ""	
	End If
	    If KeyCode = LeftMagnaSave Then 
		UltraDMD.DisplayScene00 "test5.gif", "", 0, "", 0, 14, 1500, 14
	End If
    If keycode = AddCreditKey Then
        If FreePlay=0 then 
		Credits=credits + 1
         PlaySound "coinin"
		UltraDMD.DisplayScene00 "blank.png", "Press Start", 15, "", 15, 14, 2000, 14
		End If
    End If
	If hsbModeActive = True then EnterHighScoreKey(keycode) End If
    If InProgress = False Then

	If Keycode = StartGameKey  And FreePlay=1 And ChooseMod=False Then
    UltraDMD.CancelRendering 
	ChooseMod=True
	UltraDMD.DisplayScene00ExWithId "0", FALSE, "blank.png", " FUN   Mod   PRO ", 15, -1, "<  Select  >", 15, -1, 14, 2000, 14
    DMD2 "blank.png",  "Select FUN For","Ramdom Missions"  , 3000 , UltraDmd_eNone , ""
	DMD2 "blank.png",  "Select PRO For","Progress Missions"  , 3000 , UltraDmd_eNone , ""
	UltraDMD.DisplayScene00ExWithId "0", FALSE, "blank.png", " FUN   Mod   PRO ", 15, -1, "<  Select  >", 15, -1, 14, 0, 14
	Endmusic
	Jukebox=0
	JukeTime.Enabled=False
	
	End If
   End If

   

    ' Manual Ball Control
	If keycode = 46 Then	 				' C Key
		If EnableBallControl = 1 Then
			EnableBallControl = 0
		Else
			EnableBallControl = 1
		End If
	End If
    If EnableBallControl = 1 Then
		If keycode = 48 Then 				' B Key
			If BCboost = 1 Then
				BCboost = BCboostmulti
			Else
				BCboost = 1
			End If
		End If
		If keycode = 203 Then BCleft = 1	' Left Arrow
		If keycode = 200 Then BCup = 1		' Up Arrow
		If keycode = 208 Then BCdown = 1	' Down Arrow
		If keycode = 205 Then BCright = 1	' Right Arrow
	End If
End Sub

Sub RotateLaneLightsLeft()
    Dim TempState
    TempState = Lanelight1.State
	Lanelight1.State = lanelight2.State
    lanelight2.State = Lanelight3.State
    Lanelight3.State = TempState
End Sub

Sub RotateLaneLightsRight()
    Dim TempState
	TempState = Lanelight3.State
	Lanelight3.State = lanelight2.State
    lanelight2.State = Lanelight1.State
    Lanelight1.State = TempState

End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger2.Fire
		PlaySound "plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	If keycode = LeftFlipperKey Then
	if Inprogress=true Then	
	LeftFlipper.RotateToStart
	Controller.B2SSetData 101, 0
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
	End If
	End If
	If keycode = RightFlipperKey Then
	if Inprogress=true Then	
		RightFlipper.RotateToStart
	Controller.B2SSetData 102, 0
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
	End If
	End If
    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
End Sub

	Sub DofPlungMX_hit()
			Controller.B2SSetData 322, 1     ' DOF MX Shoot
			Controller.B2SSetData 322, 0     ' DOF MX Shoot
			Controller.B2SSetData 300, 0     ' DOF MX Ball is Ready to Shoot
	End Sub


'*****GI Lights On
dim xx

For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    FlashEvent2
	RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	Controller.B2SSetData 104, 1
	Controller.B2SSetData 104, 0
	Controller.B2SSetData 303, 1   'DOF MX - Right Slingshot
	Controller.B2SSetData 303, 0   'DOF MX - Right Slingshot
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    FlashEvent2
	LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	Controller.B2SSetData 103, 1
	Controller.B2SSetData 103, 0
	Controller.B2SSetData 304, 1   'DOF MX - Left Slingshot
	Controller.B2SSetData 304, 0   'DOF MX - Left Slingshot
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:
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

Sub Trigger1_Hit()
	playsound "metalhit_thin"
	Controller.B2SSetData 128, 1 	'Launch Ball Button Flashing - ON
End Sub
Sub Trigger1_UnHit()
	Controller.B2SSetData 128, 0  'Launch Ball Button Flashing - OFF
End Sub

'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
	playsound "fx_ball_drop4"
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 8 ' total number of balls
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

    For b = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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

End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
   If shad=1 Then
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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End If
End Sub


'*************ULTRA DMD****************
Dim UltraDMD

'---------- UltraDMD Unique Table Color preference -------------
Dim DMDColor, DMDColorSelect, UseFullColor
Dim DMDPosition, DMDPosX, DMDPosY, DMDSize, DMDWidth, DMDHeight 


UseFullColor = "True"                         '  "True" / "False"
DMDColorSelect = "DarkViolet"          ' Rightclick on UDMD window to get full list of colours

'Note open Ultradmd and right click on window to get the various sizes in decimal 

' GetDMDColor
' Sub GetDMDColor
' Dim WshShell,filecheck,directory
' Set WshShell = CreateObject("WScript.Shell")
' If DMDSize then
' WshShell.RegWrite "HKCU\Software\UltraDMD\w",DMDWidth,"REG_DWORD"
' WshShell.RegWrite "HKCU\Software\UltraDMD\h",DMDHeight,"REG_DWORD"
' End if
' If DMDPosition then
' WshShell.RegWrite "HKCU\Software\UltraDMD\x",DMDPosX,"REG_DWORD"
' WshShell.RegWrite "HKCU\Software\UltraDMD\y",DMDPosY,"REG_DWORD"
' End if
' WshShell.RegWrite "HKCU\Software\UltraDMD\fullcolor",UseFullColor,"REG_SZ"
' WshShell.RegWrite "HKCU\Software\UltraDMD\color",DMDColorSelect,"REG_SZ"
' End Sub
'---------------------------------------------------


Sub LoadUltraDMD
    Set UltraDMD = CreateObject("UltraDMD.DMDObject")
    If UltraDMD is Nothing Then
        MsgBox "No UltraDMD found.  This table MAY run without it."
        Exit Sub
    End If

    UltraDMD.Init
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If

    If UltraDMD.GetMinorVersion < 1 Then
        MsgBox "Incompatible Version of UltraDMD found. Please update to version 1.1 or newer."
        Exit Sub
    End If

    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir
    curDir = fso.GetAbsolutePathName(".")
    UltraDMD.SetProjectFolder curDir & "\DarkPrincess.UltraDMD"
End Sub

Sub OnScoreboardChanged()
    'If we're in the middle of a scene, don't interrupt it to display the scoreboard.
    If Not UltraDMD.IsRendering Then
        UltraDMD.DisplayScoreboard Score, "credits " & CStr(Credits), "ball " & CStr(Ball)
    End If
End Sub

Sub UltraDMD_SetScoreboardBackground(imageName)
	If Not UltraDMD is Nothing Then
		UltraDMD.SetScoreboardBackgroundImage imageName, 15, 15
	End If
End Sub

Sub RunAllScenes
            
			'UltraDMD.DisplayScene00ExWithId "my_scene_0001", FALSE, "", "Dark", 15, -1, "Princess", -1, -1, 14, 2000, 14
            DMD2 "FROGGY.png", "","",3000,UltraDmd_eNone,"" 
			UltraDMD.DisplayScene00 "test5.gif", "", 0, "", 0, 14, 3500, 14
			DMD2 "PRESSJUKE.png", "" , "" , 3500 , UltraDmd_eNone , ""
			DMD2 "blank.png", "HIGH SCORES", "", 2000, UltraDmd_eNone , ""
			DMD2 "blank.png",  "1 - " + HighScoreName(0),HighScore(0)  , 3000 ,UltraDmd_eNone , ""
			DMD2 "blank.png",  "2 - " + HighScoreName(1),HighScore(1)  , 3000 ,UltraDmd_eNone , ""
			DMD2 "blank.png",  "3 - " + HighScoreName(2),HighScore(2)  , 3000 ,UltraDmd_eNone , ""
			DMD2 "blank.png",  "4 - " + HighScoreName(3),HighScore(3)  , 3000 , UltraDmd_eNone , ""	

			DMD2 "reset.png", "" , "" , 2500 , UltraDmd_eNone , ""
			DMD2 "blank.png", "PRESS" , "START" , 500 , UltraDmd_eNone , ""
			DMD2 "blank.png", "" , "" , 500 , UltraDmd_eNone , ""
			DMD2 "blank.png", "PRESS" , "START" , 500 , UltraDmd_eNone , ""
			DMD2 "blank.png", "" , "" , 500 , UltraDmd_eNone , ""
			DMD2 "blank.png", "PRESS" , "START" , 500 , UltraDmd_eNone , ""
			DMD2 "blank.png", "" , "" , 500 , UltraDmd_eNone , ""
			DMD2 "blank.png", "PRESS" , "START" , 500 , UltraDmd_eNone , ""

End Sub

SUB TimerDmdIntro_Timer
If ChooseMod=False then
RunAllScenes
End IF
End Sub

Sub DMDScore()
	If Freeplay=1 Then
	UltraDMD.SetScoreboardBackgroundImage "bg01.png", 15, 15
	UltraDMD.DisplayScoreboard 1, 1, "" & Score, 2000000, 3000000, 4000000, "Ball:"& Ball, "Extra:" & Extraball
	End If
	If Freeplay=0 Then
	UltraDMD.SetScoreboardBackgroundImage "bg01.png", 15, 15
	UltraDMD.DisplayScoreboard 1, 1, "" & Score, 2000000, 3000000, 4000000, "Ball:"& Ball, "Credits:" & Credits
	End If
End Sub

Sub SKillshotScene
     UltraDMD.DisplayScene00 "skillshot.gif", "", 0, "", 0, 14, GifTime1, 14
If Voice =1 Or Voice=2 Then playsound "SkillF",0,BGlevel End If
If Voice =0 Then playsound "SkillM",0,BGlevel End If
	DMDScore
	Controller.B2SSetData 361, 1    ' DOF MX Skillshot
	Controller.B2SSetData 361, 0    ' DOF MX Skillshot
End Sub


'**************ATRAQMODE************
Sub StartLightSeq()
    'lights sequences
	Controller.B2SStartAnimation "Attract"
	TimerFlash.Enabled = True
	LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
  
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

'*************Table Info*****************
'*************Lane triggers**************
Dim BonusMult


Sub Lanetrig1_hit()
	AddScore 500
	addbonus 50
	If TSkillshot.Enabled=False Then
		Lanelight1.State=1
	Controller.B2SSetData 325, 1    ' DOF MX Top Rollover 1
	Controller.B2SSetData 325, 0    ' DOF MX Top Rollover 1
	End If
	If Lanelight2.State=1 And lanelight3.State=1 Then
		BonusMult=BonusMult +1
		Lanetimer.Enabled=True
		Lanetrig1.Enabled=False
		Lanetrig2.Enabled=False
		Lanetrig3.Enabled=False

	End If
	If TSkillshot.Enabled=True And Lanelight1.State=2 And SS=0 Then
		SS=SS+1
		AddScore 50000
		SKillshotScene
		DMDScore
		Lanelight1.State=1
		End If
		CheckXbonus
End Sub

Sub Lanetrig2_hit()
		AddScore 500
		addbonus 50
	If TSkillshot.Enabled=False Then
		Lanelight2.State=1
	Controller.B2SSetData 326, 1    ' DOF MX Top Rollover 2
	Controller.B2SSetData 326, 0    ' DOF MX Top Rollover 2
	End If
	If Lanelight1.State=1 And lanelight3.State=1 Then
		BonusMult=BonusMult +1
	Lanetimer.Enabled=True
		Lanetrig1.Enabled=False
		Lanetrig2.Enabled=False
		Lanetrig3.Enabled=False
	End If
	
	If TSkillshot.Enabled=True And lanelight2.State=2 And SS=0 Then
		SS=SS+1
		AddScore 50000
		SKillshotScene
		DMDScore
		lanelight2.State=1
	End If
		CheckXbonus
End Sub

Sub Lanetrig3_hit()
		AddScore 500
		addbonus 50
	If TSkillshot.Enabled=False Then
		Lanelight3.State=1
	Controller.B2SSetData 327, 1    ' DOF MX Top Rollover 3
	Controller.B2SSetData 327, 0    ' DOF MX Top Rollover 3
	End If
	If Lanelight2.State=1 And Lanelight1.State=1 Then
		BonusMult=BonusMult +1
	Lanetimer.Enabled=True
		Lanetrig1.Enabled=False
		Lanetrig2.Enabled=False
		Lanetrig3.Enabled=False

	End If
	If TSkillshot.Enabled=True And Lanelight3.State=2 And SS=0 Then
		SS=SS+1
		AddScore 50000
		SKillshotScene
		DMDScore
		Lanelight3.State=1
		End If
		CheckXbonus
End Sub

Sub Lanetimer_Timer
		lanetimer.Enabled=False
		Lanelight1.State=0
		lanelight2.State=0
		Lanelight3.State=0
		Lanetrig1.Enabled=True
		Lanetrig2.Enabled=True
		Lanetrig3.Enabled=True
End Sub
'****************BALLSave****************
dim bsa
Sub Gate_Hit()
		PlaySound "gate4"
		Addscore 10
		addscore -10
		Klight001.State=1
		Klight002.State=1
		Controller.B2SStopAnimation "Attract"
		
	IF bsa=0 And Tballsave2.Enabled=False Then
		TBallsave.Enabled=True
		BallsaveLight.State=1
	End If
	If SS=0 Then
		TSkillshot.Enabled=True
	End If


End Sub

Sub Gate003_Hit()
		PlaySound "gate4"
		Addscore 10
		LightSeqRamp.StopPlay
	End Sub

Sub TBallsave_timer()
		TBallsave.Enabled=False
		BallsaveLight.State=0
End Sub

'*************SKILLSHOT**********
Dim SS
Sub TSkillshot_timer()
TSkillshot.Enabled=False
Lanelight1.State=1
lanelight2.State=0
Lanelight3.State=0
End Sub

'************Bumper**************
Dim B1
Dim B2

Sub Bumper001_hit()
	If Moveit=1 Then : Princess3d.ObjRotZ = (Princess3d.ObjRotZ + 1) : End If
		if InProgress=False then 
		End If
		Flashevent
		BPL1.State=1
		BPL1b.State=1
		thunder01.State=1
		Controller.B2SSetData 107, 1
		Controller.B2SSetData 107, 0
	    Controller.B2SSetData 305, 1    ' DOF MX Bumper 1 
	    Controller.B2SSetData 305, 0    ' DOF MX Bumper 1
		Me.TimerEnabled=1
		B1=B1+1
		addscore 1000
		Addbonus 100
		DMDScore
		PlaySound "FloorTom2",0,Blevel
		PlaySound "fx_bumper1"
		
	If B1=50 Then
        UltraDMD.DisplayScene00 "BumpersBonus.gif", "", 0, "", 0, 14, GifTime1, 14
		AddScore 25000
		Addbonus 2500
		B2=B2+1
		B1=0
		playSound "Crash",0,BGlevel
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 355, 1	' DOF MX Bumper 
		Controller.B2SSetData 355, 0	' DOF MX Bumper 
		Flashevent3
	End If
	If B2=2 Then
        UltraDMD.DisplayScene00 "BumpersBonus.gif", "", 0, "", 0, 14, GifTime1, 14
		addScore 50000
		B2=0
		playSound "Crash",0,BGlevel
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 355, 1	' DOF MX Bumper 
		Controller.B2SSetData 355, 0	' DOF MX Bumper 
		Flashevent3
		If Extraball<2 And Mextra<2  Then
		Lextra1.State=1
If Voice =1 Then playsound "ExtralitF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "ExtralitM",0,BGlevel End If
		UltraDMD.DisplayScene00 "ExtraballIsLite.gif", "", 0, "", 0, 14, GifTime1, 14
		End If
	End If
End Sub

Sub Bumper001_timer
		BPL1.State=0
		BPL1b.State=0
		thunder01.State=0
		Me.timerEnabled=0
End Sub

Sub Bumper002_hit()
	If Moveit=1 Then : Princess3d.ObjRotZ = (Princess3d.ObjRotZ - 3) : End If
		if InProgress=False then 
		End If
		Flashevent
		BPL2.State=1
		BPL2b.State=1
		thunder02.State=1
		Controller.B2SSetData 108, 1
		Controller.B2SSetData 108, 0
	    Controller.B2SSetData 306, 1    ' DOF MX Bumper 2
	    Controller.B2SSetData 306, 0    ' DOF MX Bumper 2
		Me.TimerEnabled=1
		B1=B1+1
		addscore 1000
		Addbonus 100
		DMDScore
		PlaySound "FloorTom3",0,Blevel
			PlaySound "fx_bumper1"
	If B1=50 Then
        UltraDMD.DisplayScene00 "BumpersBonus.gif", "", 0, "", 0, 14, GifTime1, 14
		UltraDMD.DisplayScoreboard 1, 1, "" & Score, 2000000, 3000000, 4000000, "BALL "& Ball, "CREDITS:" & Credits
		AddScore 25000
		Addbonus 2500
		B2=B2+1
		B1=0
		playSound "Crash",0,BGlevel
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 355, 1 	' DOF MX Bumper 
		Controller.B2SSetData 355, 0	' DOF MX Bumper

		Flashevent3
	End If
	If B2=2 Then
        UltraDMD.DisplayScene00 "BumpersBonus.gif", "", 0, "", 0, 14, GifTime1, 14
		addScore 50000
		B2=0
		playSound "Crash",0,BGlevel
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 355, 1 	' DOF MX Bumper 
		Controller.B2SSetData 355, 0	' DOF MX Bumper
		Flashevent3
		If Extraball<2 And Mextra<2 Then
		Lextra1.State=1
If Voice =1 Or Voice=2 Then playsound "ExtralitF",0,BGlevel End If
If Voice =0 Then playsound "ExtralitM",0,BGlevel End If

		UltraDMD.DisplayScene00 "ExtraballIsLite.gif", "", 0, "", 0, 14, GifTime1, 14
		End If
	End If
End Sub

Sub Bumper002_timer
	BPL2.State=0
	BPL2b.State=0
	thunder02.State=0
	Me.timerEnabled=0
End Sub

Sub Bumper003_hit()
	If Moveit=1 Then : Princess3d.ObjRotZ = (Princess3d.ObjRotZ + 1) End If
		if InProgress=False then 
		End If
		Flashevent
		BPL3.State=1
		BPL3b.State=1
		thunder03.State=1
		Controller.B2SSetData 109, 1
		Controller.B2SSetData 109, 0
	    Controller.B2SSetData 307, 1    ' DOF MX Bumper 3
	    Controller.B2SSetData 307, 0    ' DOF MX Bumper 3
		Me.TimerEnabled=1
		B1=B1+1
		addscore 1000
		Addbonus 100
		DMDScore
		PlaySound "FloorTom4",0,Blevel
		PlaySound "fx_bumper1"
	If B1=50 Then
        UltraDMD.DisplayScene00 "BumpersBonus.gif", "", 0, "", 0, 14, GifTime1, 14
		UltraDMD.SetScoreboardBackgroundImage "bg01.png", 15, 15
		UltraDMD.DisplayScoreboard 1, 1, "" & Score, 2000000, 3000000, 4000000, "BALL "& Ball, "CREDITS:" & Credits
		AddScore 25000
		Addbonus 2500
		B2=B2+1
		B1=0
		Playsound "Crash",0,BGlevel
		Flashevent3
		Controller.B2SSetData 355, 1	' DOF MX Bumper 
		Controller.B2SSetData 355, 0	' DOF MX Bumper 
	End If
	If B2=2 Then
        UltraDMD.DisplayScene00 "BumpersBonus.gif", "", 0, "", 0, 14, GifTime1, 14
		UltraDMD.SetScoreboardBackgroundImage "bg01.png", 15, 15
		UltraDMD.DisplayScoreboard 1, 1, "" & Score, 2000000, 3000000, 4000000, "BALL "& Ball, "CREDITS:" & Credits
		addScore 50000
		B2=0
		playSound "Crash",0,BGlevel
		Flashevent3
		If Extraball<2 And Mextra<2 Then
		Lextra1.State=1
If Voice =1 Then playsound "ExtralitF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "ExtralitM",0,BGlevel End If
		UltraDMD.DisplayScene00 "ExtraballIsLite.gif", "", 0, "", 0, 14, GifTime1, 14
		Controller.B2SSetData 355, 1	' DOF MX Bumper 
		Controller.B2SSetData 355, 0	' DOF MX Bumper 
		End If
	End If
End Sub

Sub Bumper003_timer
		BPL3.State=0
		BPL3b.State=0
		thunder03.State=0
		Me.timerEnabled=0
End Sub



	'******************RAMP*************
Sub RightInlane_Hit
		LightSeqRamp.StopPlay
		StopSound "fx_metalrolling"
Controller.B2SSetData 120, 0
Controller.B2SSetData 328, 1    ' DOF MX Right InnerLane Rollover
Controller.B2SSetData 328, 0    ' DOF MX Right InnerLane Rollover
Controller.B2SSetData 330, 0    ' DOF MX Ramp Electrical Effects
	End Sub

Dim Rampb
Dim M4R
Dim Extraball


Sub RampStart_hit()
		PlaySound "fx_FlatRampRoll",0,1,-0.8,0,1,1
End Sub

Sub RampStop_hit()
		StopSound "fx_FlatRampRoll"
End Sub


Sub RampTR_hit()
		StopSound "fx_FlatRampRoll"
		PlaySound "fx_metalrolling",0, 0.98, 0.8
		Flashevent3
		PlaySound "FoudreRamp",0,BGlevel
		Rampb=rampb+1
		LightSeqRamp.Play SeqDownOn, 10
				Controller.B2SSetData 120, 1
			    Controller.B2SSetData 324, 1    ' DOF MX Ramp Navigate
			    Controller.B2SSetData 324, 0    ' DOF MX Ramp Navigate
			    Controller.B2SSetData 330, 1    ' DOF MX Ramp Electrical Effects	
		
	If Ballinpf>1 And Multijacklight.State=1 Then
			If Moveit=1 then Timer8.Enabled=True End If
			Addscore 15000
			UltraDMD.DisplayScene00 "JJackpot.gif", "", 0, "", 0, 14, GifTime1, 14
			Controller.B2SSetData 360, 1    ' DOF MX Jackpot
			Controller.B2SSetData 360, 0    ' DOF MX Jackpot
	End If
	If Rampb=1 Then
		LRamp1.State=1
		AddScore 5000
		Addbonus 500
	End If
	If Rampb=2 Then
		LRamp2.State=1
		AddScore 5000
		Addbonus 500
	End If
	If Rampb=3 Then
		LRamp3.State=1
		AddScore 5000
		Addbonus 500
	End If
	If Rampb=4 Then
		LRamp4.State=1
		AddScore 5000
		Addbonus 500
	End If
	If Rampb=5 Then
		LRamp5.State=1
		AddScore 5000
		Addbonus 500
	End If
	If Rampb=6 Then
		LRamp6.State=1
		AddScore 20000
		Addbonus 2000
		resetlightramp
		If Extraball<2 And Mextra<2 Then
		Lextra1.State=1
If Voice =1 Then playsound "ExtralitF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "ExtralitM",0,BGlevel End If
		UltraDMD.DisplayScene00 "ExtraballIsLite.gif", "", 0, "", 0, 14, GifTime1, 14
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		End If
	End If

	If Rampb=12 Then
		LRamp6.State=1
		AddScore 100000
		Addbonus 10000
		resetlightramp
				UltraDMD.DisplayScene00 "JJackpot.gif", "", 0, "", 0, 14, GifTime1, 14
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 360, 1    ' DOF MX Jackpot
		Controller.B2SSetData 360, 0    ' DOF MX Jackpot
	End If
	If Rampb=18 Then
		LRamp6.State=1
		AddScore 200000
		Addbonus 20000
		resetlightramp
		If Extraball<2 And Mextra<2 Then
		Lextra1.State=1
		playsound "extralit",0,BGlevel

		UltraDMD.DisplayScene00 "ExtraballIsLite.gif", "", 0, "", 0, 14, GifTime1, 14
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		End If
		End If
	If Rampb=24 Then
		LRamp6.State=1
		AddScore 200000
		Addbonus 20000
		resetlightramp
		UltraDMD.DisplayScene00 "JJackpot.gif", "", 0, "", 0, 14, GifTime1, 14
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 360, 1    ' DOF MX Jackpot
		Controller.B2SSetData 360, 0    ' DOF MX Jackpot
	End If	
	If Rampb=30 Then
		LRamp6.State=1
		AddScore 500000
		Addbonus 50000
		resetlightramp
		If Extraball<2 And Mextra<2 Then
		Lextra1.State=1
If Voice =1 Or Voice=2 Then playsound "ExtralitF",0,BGlevel End If
If Voice =0 Then playsound "ExtralitM",0,BGlevel End If
		UltraDMD.DisplayScene00 "ExtraballIsLite.gif", "", 0, "", 0, 14, GifTime1, 14
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 115, 1
		Controller.B2SSetData 115, 0
		End If
	End If
		If Rampb=36 Then
		LRamp6.State=1
		AddScore 500000
		Addbonus 50000
		resetlightramp
		UltraDMD.DisplayScene00 "JJackpot.gif", "", 0, "", 0, 14, GifTime1, 14
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 115, 1
		Controller.B2SSetData 115, 0
		Controller.B2SSetData 360, 1    ' DOF MX Jackpot
		Controller.B2SSetData 360, 0    ' DOF MX Jackpot
		End If	
		If Rampb=42 Then
		LRamp6.State=1
		AddScore 500000
		Addbonus 50000
		resetlightramp
		TELSA.Enabled=True
		Rampb=0
		UltraDMD.DisplayScene00 "MultiBall.gif", "", 0, "", 0, 14, GifTime1, 14
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 115, 1
		Controller.B2SSetData 115, 0
	End If	
		
		
	If M2Inprogress=True And LT4.State=1 Then
		LT4.State=0
		AddScore 2000
		Addbonus 200
		Checkmission2
	End If
	If M4Inprogress=True And LT4.State=1  Then
		AddScore 4000
		Addbonus 400
		LT4.State=0
		CheckMission4
	End If

	If M5Inprogress=True And LT4.State=1 Then
		LT4.State=0
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT4.State=1 Then
		LT4.State=0
		addscore 5000
		addbonus 500
		CheckMission6
	End If
		If M8Inprogress=True And LT4.State=1 and M8=1 Then
		LT4.State=0
		M8=M8+1
		LT6.State=1
		PlaySound "sword",0,BGlevel
		addscore 4000
		addbonus 400
	End If
		if M10Inprogress=True And LT4.State=1 And M10=1 Then
		LT4.State=0	
		LT10.State=1
		M10=M10+1
		PlaySound "sword",0,BGlevel
		Addscore 8000
		Addbonus 800
	End If
	If M11Inprogress=True And LT4.State=1 And M11=3 Then
		LT4.State=0
		LT5.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If
End Sub
	


Sub resetlightramp()
		Lramp1.State=0
		LRamp2.State=0
		LRamp3.State=0
		Lramp4.State=0
		LRamp5.State=0
		LRamp6.State=0
End Sub


'******************KickerMission***************
Dim KickerM
Dim Kick
Dim M7
DIM M8
DIM M9
DIM M10
DIM M11
DIM M12
Sub kickermission_hit()
	If MissionInProgress=False Then
		kick=kick+1
		AddScore 1000
		Addbonus 100
		TMission.Interval = 3000
		TMission.Enabled=True
	End If
	If MissionInProgress=True Then
		TMission.Interval = 500
		Tmission.Enabled=True
		DMDFLush
	End If


	If Missioninprogress=False And Kick=1 Then
		PlaySound "foudre",0,BGlevel
If Voice =1 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		LT001.State=0
		LStart.State=0
		If Glevel=0 Then
 ImageMission 
End If
		If Glevel=1 Then
 Mission1 
End If
		

	End if
	
	If MissionInProgress=False And Kick=2 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	
	If MissionInProgress=False And Kick=3 Then
		PlaySound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		LT001.State=0
		LStart.State=0
		If Glevel=0 Then
 ImageMission
 End If
		If Glevel=1 Then
 Mission2 
End If


	End If
	If MissionInProgress=False And Kick=4 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And Kick=5 Then
		PlaySound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		LT001.State=0
		Lstart.State=0
		If Glevel=0 Then
 ImageMission
 End If
		If Glevel=1 Then
 Mission3 
End If


	End If
	If MissioninProgress=False And kick=6 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=7 Then
		Lt001.State=0
		Lstart.State=0
		If Glevel=0 Then
 ImageMission 
End If
		If Glevel=1 Then 
Mission4 
End If
		PlaySound "foudre",0,BGlevel
If Voice =1 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0

	End If
	If MissionInProgress=False And Kick=8 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=9 Then	
		PlaySound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		Lt001.State=0
		Lstart.State=0
		If Glevel=0 Then 
ImageMission 
End If
		If Glevel=1 Then
Mission5 
End If

	End If
	If MissionInProgress=False And Kick=10 Then
		LT001.State=1
		Light003.State=2
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=11 Then
		Lt001.State=0
		Light003.State=2
		lstart.State=0
		Mission6
		PlaySound "foudre",0,BGlevel
If Voice =1 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "ShootM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 395, 1     ' DOF MX Elsa
		CercleSeq.Play SeqClockRightOn,0
		UltraDMD.DisplayScene00 "P-Elsa.gif", "", 0, "", 0, 14, GifTime2, 14

	End If
	If MissionInProgress=False And Kick=12 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=13 Then	
		PlaySound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		Lt001.State=0
		Lstart.State=0
		If Glevel=0 Then
 ImageMission2 
End If
		If Glevel=1 Then
 Mission7 
End If
	End If
	If MissionInProgress=False And Kick=14 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=15 Then	
		PlaySound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		Lt001.State=0
		Lstart.State=0
		If Glevel=0 Then
 ImageMission2
 End If
		If Glevel=1 Then
 Mission8 
End If

	End If
	If MissionInProgress=False And Kick=16 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=17 Then	
		PlaySound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		Lt001.State=0
		Lstart.State=0
		If Glevel=0 Then
 ImageMission2 
End If
		If Glevel=1 Then
 Mission9 
End If

	End If
	If MissionInProgress=False And Kick=18 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=19 Then	
		PlaySound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		Lt001.State=0
		Lstart.State=0
		If Glevel=0 Then
 ImageMission2 
End If
		If Glevel=1 Then
 Mission10 
End If

	End If
	If MissionInProgress=False And Kick=20 Then
		LT001.State=1
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=21 Then	
		PlaySound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Then playsound "ShootM",0,BGlevel End If
		CercleSeq.Play SeqClockRightOn,0
		Lt001.State=0
		Lstart.State=0
		If Glevel=0 Then
 ImageMission2 
End If
		If Glevel=1 Then
 Mission11 
End If

	End If

If MissionInProgress=False And Kick=22 Then
		LT001.State=1
		Light003.State=2
		Lstart.State=1
		LockFlash
	End If
	If MissionInProgress=False And kick=23 Then
		Lt001.State=0
		Light003.State=2
		lstart.State=0
		Mission6
		PlaySound "foudre",0,BGlevel
If Voice =1 Then playsound "ShootF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "ShootM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 395, 1     ' DOF MX Elsa
		CercleSeq.Play SeqClockRightOn,0
		UltraDMD.DisplayScene00 "P-Elsa.gif", "", 0, "", 0, 14, GifTime2, 14
		kick=0
	End If

	If Lextra1.State=1  Then
		UltraDMD.DisplayScene00 "Extraball.gif", "", 0, "", 0, 14, GifTime1, 14
		AddScore 5000
		Addbonus 500
		LRamp1.State=0
		LRamp2.State=0
		LRamp3.State=0
		Lramp4.State=0
		LRamp5.State=0
		LRamp6.State=0
		LExtra1.State=0
		ExtraBLight.State=1
If Voice =1 Then playsound "ExtraF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "ExtraM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 356, 1	' DOF MX Extraball 
		Controller.B2SSetData 356, 0	' DOF MX Extraball
		Extraball=Extraball+1
		Mextra=Mextra+1
		FlashMission
		
	End if

End Sub

Sub TMission_Timer()
		Tmission.Enabled=False
		Kickermission.Kick 320,50
		playSound "ballrelease"
		Controller.B2SSetData 111, 1
		Controller.B2SSetData 111, 0
		Controller.B2SSetData 308, 1     ' DOF MX Kicker
		Controller.B2SSetData 308, 0     ' DOF MX Kicker
End Sub

Sub Mission1()
		UltraDMD.DisplayScene00 "P-Rapunzel.gif", "", 0, "", 0, 14, GifTime2, 14
		Flashevent3
		MissionInProgress=True
		M1Inprogress=True
		EndMusic
		DPAlice_MusicDone
		LM22.State=2
		LT1.State=1
		LT5.State=1
		LT8.State=1
		LT10.State=1
		FlashMission
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 390, 1     ' DOF MX Rapunzel
End Sub

Sub Checkmission1()
	If LT1.State=0 And LT5.State=0 And LT8.State=0 And LT10.State=0 Then
		UltraDMD.DisplayScene00 "P2-Rapunzel-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 50000
		Addbonus 5000
		DMDScore
		M1Inprogress=False
		MissionInProgress=False
		LM22.State=1
		LStart.State=2
		Playsound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 390, 0     ' DOF MX Rapunzel
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10
	End IF 
End Sub

Sub Mission2()
		UltraDMD.DisplayScene00 "P-Ariel.gif", "", 0, "", 0, 14, GifTime2, 14
		Flashevent3
		MissionInProgress=True
		M2Inprogress=True
		EndMusic
		DPAriel_MusicDone
		LM32.State=2
		LT2.State=1
		LT4.State=1
		LT6.State=1
		LT11.State=1
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
	    Controller.B2SSetData 391, 1     ' DOF MX Ariel
End Sub

Sub Checkmission2()
	If LT2.State=0 And LT4.State=0 And LT6.State=0 And LT11.State=0 Then
		UltraDMD.DisplayScene00 "P2-Ariel-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 100000
		Addbonus 10000
		DMDScore
		M2Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 391, 0     ' DOF MX Ariel
		LM32.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10
	End IF 
End Sub

Sub Mission3()
		UltraDMD.DisplayScene00 "P-Beauty.gif", "", 0, "", 0, 14, GifTime2, 14	
		Flashevent3
		MissionInProgress=True
		M3Inprogress=True
		EndMusic
		DpBeauty_MusicDone
		LM42.State=2
		LT1.State=1
		LT2.State=1
		LT3.State=1
		LT9.State=1
		LT10.State=1
		LT11.State=1
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 392, 1     ' DOF MX Beauty
End Sub

Sub CheckMission3()
	If LT1.State=0 And LT2.State=0 And LT3.State=0 And LT9.State=0 And LT10.State=0 And LT11.State=0 Then
		UltraDMD.DisplayScene00 "P2-Beauty-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 150000
		Addbonus 15000
		DMDScore
		M3Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 392, 0     ' DOF MX Beauty
		LM42.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10

	End IF 
End Sub


Sub Mission4()
		UltraDMD.DisplayScene00 "P-Snow.gif", "", 0, "", 0, 14, GifTime2, 14
		Flashevent3
		MissionInProgress=True
		M4Inprogress=True
		EndMusic
		DPSnow_MusicDone
		LM002.State=2
		LT1.State=1
		LT2.State=1
		LT3.State=1
		LT4.State=1'2fois
		LT9.State=1
		LT10.State=1

		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 393, 1     ' DOF MX Snow

End Sub

Sub CheckMission4()
	If LT1.State=0 And LT2.State=0 And LT3.State=0 And LT4.State=0 And LT9.State=0 And LT10.State=0 Then
		UltraDMD.DisplayScene00 "P2-Snow-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 200000
		Addbonus 20000
		DMDScore
		M4Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 393, 0     ' DOF MX Snow
		LM002.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10
	End IF 
End Sub

Sub Mission5()
		UltraDMD.DisplayScene00 "P-Cinderella.gif", "", 0, "", 0, 14, GifTime2, 14		
		Flashevent3
		MissionInProgress=True
		M5Inprogress=True
		EndMusic
		DPAURORE_MusicDone
		LM62.State=2
		LT1.State=1
		LT2.State=1
		LT3.State=1
		LT4.State=1
		LT5.State=1
		LT6.State=1
		LT8.State=1
		LT9.State=1
		LT10.State=1
		LT10.State=1
		LT11.State=1
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 394, 1     ' DOF MX Cinderella

End Sub

Sub CheckMission5()
	If LT1.State=0 And LT2.State=0 And LT3.State=0 And LT4.State=0 And LT5.State=0 And LT6.State=0 And LT8.State=0 And LT9.State=0 And LT10.State=0 And LT11.State=0 Then
		UltraDMD.DisplayScene00 "P2-Cinderella-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 250000
		Addbonus 25000
		DMDScore
		M5Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 394, 0     ' DOF MX Cinderella
		LM62.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10
	End IF 
End Sub

Sub Mission6()
		Flashevent3
		MissionInProgress=True
		M6Inprogress=True
		EndMusic
		DPElsa_MusicDone
		LT1.State=1
		LT2.State=1
		LT3.State=1
		LT4.State=1
		LT5.State=1
		LT6.State=1
		LT8.State=1
		LT9.State=1
		LT10.State=1
		LT11.State=1
		TELSA.Enabled=True
		BallsaveLight.State=1
		Light003.State=2
End Sub

Sub Tballsave2_Timer
Tballsave2.enabled=False
BallsaveLight.State=0
End Sub


Dim M
Sub TELSA_Timer()
		TELSA.Enabled=False
		M=M+1
		If M <5 Then
		Controller.B2SSetData 111, 1
		Controller.B2SSetData 111, 0
		PlaySound "ballrelease"
		kicker1.Createball
		kicker1.Kick 90,7
		Plunger.PullBack
		Plungertime.Enabled=True
		Ballinpf=ballinpf+1
		TELSA.Enabled=True
		End If
		If M=5 Then
		M=0
		Exit Sub
		End IF
		Multijacklight.State=1
		TBallsave.Enabled=False
		Tballsave2.Enabled=True
		If MissionInProgress=False Then
		UltraDMD.DisplayScene00 "MultiBall.gif", "", 0, "", 0, 14, GifTime1, 14
		End If
		Controller.B2SSetData 367, 1	' DOF MX Multi
		Controller.B2SSetData 367, 0	' DOF MX Multi

End Sub

Sub CheckMission6()
	If LT1.State=0 And LT2.State=0 And LT3.State=0 And LT4.State=0 And LT5.State=0 And LT6.State=0 And LT8.State=0 And LT9.State=0 And LT10.State=0 And LT11.State=0 Then
		UltraDMD.DisplayScene00 "P2-Elsa-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 1000000
		Addbonus 100000
		DMDScore
		M6Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 395, 0     ' DOF MX Elsa
		Light003.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10
		ResetMissionTimer.Enabled=True
		missionDouble=True
	End IF 
End Sub

Sub Mission7()
		UltraDMD.DisplayScene00 "P-Rapunzel.gif", "", 0, "", 0, 14, GifTime2, 14
		Flashevent3
		MissionInProgress=True
		M7Inprogress=True
		EndMusic
		DPAlice_MusicDone
		LM22.State=2
		LT1.State=1
		End Sub
Sub Checkmission7
UltraDMD.DisplayScene00 "P2-Rapunzel-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 100000
		Addbonus 10000
		DMDScore
		M7Inprogress=False
		MissionInProgress=False
		LM22.State=1
		LStart.State=2
		Playsound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 390, 0     ' DOF MX Rapunzel
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10

End Sub

Sub Mission8()
		UltraDMD.DisplayScene00 "P-Ariel.gif", "", 0, "", 0, 14, GifTime2, 14
		Flashevent3
		MissionInProgress=True
		M8Inprogress=True
		EndMusic
		DPAriel_MusicDone
		LM32.State=2
		LT2.State=1
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
	    Controller.B2SSetData 391, 1     ' DOF MX Ariel
End Sub

Sub Checkmission8()
		UltraDMD.DisplayScene00 "P2-Ariel-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 200000
		Addbonus 30000
		DMDScore
		M8Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 391, 0     ' DOF MX Ariel
		LM32.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10 
End Sub

Sub Mission9()
		UltraDMD.DisplayScene00 "P-Beauty.gif", "", 0, "", 0, 14, GifTime2, 14	
		Flashevent3
		MissionInProgress=True
		M9Inprogress=True
		EndMusic
		DpBeauty_MusicDone
		LM42.State=2
		LT9.State=1
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 392, 1     ' DOF MX Beauty
End Sub

Sub CheckMission9()
		UltraDMD.DisplayScene00 "P2-Beauty-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 300000
		Addbonus 30000
		DMDScore
		M9Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 392, 0     ' DOF MX Beauty
		LM42.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10


End Sub

Sub Mission10()
		UltraDMD.DisplayScene00 "P-Snow.gif", "", 0, "", 0, 14, GifTime2, 14
		Flashevent3
		MissionInProgress=True
		M10Inprogress=True
		EndMusic
		DPSnow_MusicDone
		LM002.State=2
		LT2.State=1


		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 393, 1     ' DOF MX Snow

End Sub

Sub CheckMission10()
		UltraDMD.DisplayScene00 "P2-Snow-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 400000
		Addbonus 40000
		DMDScore
		M10Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 393, 0     ' DOF MX Snow
		LM002.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10
 
End Sub

Sub Mission11()
		UltraDMD.DisplayScene00 "P-Cinderella.gif", "", 0, "", 0, 14, GifTime2, 14		
		Flashevent3
		MissionInProgress=True
		M11Inprogress=True
		EndMusic
		DPAURORE_MusicDone
		LM62.State=2
		LT1.State=1
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 394, 1     ' DOF MX Cinderella

End Sub

Sub CheckMission11()
		UltraDMD.DisplayScene00 "P2-Cinderella-Compl.gif", "", 0, "", 0, 14, GifTime2, 14
		ADDSCORE 500000
		Addbonus 50000
		DMDScore
		M11Inprogress=False
		MissionInProgress=False
		Playsound "foudre",0,BGlevel
If Voice =1 Or Voice=2 Then playsound "CollectF",0,BGlevel End If
If Voice =0 Then playsound "CollectM",0,BGlevel End If
		Controller.B2SSetData 150, 1
		Controller.B2SSetData 150, 0
		Controller.B2SSetData 394, 0     ' DOF MX Cinderella
		LM62.State=1
		LStart.State=2
		FlashMission
		LightSeqRamp.Play SeqUpOn, 10 
End Sub


Sub ResetMissionTimer_Timer
ResetMissionTimer.Enabled=False
		LM22.State=0
		LM32.State=0
		LM42.State=0
		LM002.State=0
		LM62.State=0
End Sub
		


'******************TARGET************
Sub JackTimer1_Timer
JackTimer1.Enabled=False
LJE1.State=0
LJE2.State=0
LJE3.State=0

End Sub

Sub JackTimer2_Timer
JackTimer2.Enabled=False
Jacklight.State=0
DoubleJacklight.State=0
Superjacklight.State=0
End Sub





Sub Target001_hit()
		FlashEvent2
		PlaySound "metalhit_medium"
		Controller.B2SSetData 310, 1     ' DOF MX Target 1
		Controller.B2SSetData 310, 0     ' DOF MX Target 1
		If TDOF=1 then
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		End If
		AddScore 500
		Addbonus 50
		Tlight001.State=1
		checkTartgetmod

	If TB=0 Then
	LJE1.State=2
	jackTimer1.Enabled=True
	End IF
	If TB=1 Then
	LJE1.State=2
	LJE2.State=2
	JackTimer1.Enabled=True
	End If
	If TB=2 Then
	LJE1.State=2
	LJE2.State=2
	LJE3.State=2
	JackTimer1.Enabled=True
	End If
	

	If M1Inprogress=True And LT1.State=1 Then
		LT1.State=0
		PlaySound "sword",0,BGlevel
		addscore 1000
		addbonus 100
		Checkmission1
	End If

	If M3Inprogress=True And LT1.State=1 Then
		LT1.State=0
		PlaySound "sword",0,BGlevel
		Addscore 3000
		Addbonus 300
		CheckMission3
	End If

	If M4Inprogress=True And LT1.State=1 Then
		LT1.State=0
		PlaySound "sword",0,BGlevel
		Addscore 4000
		Addbonus 400
		CheckMission4
	End If
	If M5Inprogress=True And LT1.State=1 Then
		LT1.State=0
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT1.State=1 Then
		LT1.State=0
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
	If M7Inprogress=True And LT1.State=1 and M7=0 Then
		LT1.State=0
		M7=M7+1
		LT5.State=1
		PlaySound "sword",0,BGlevel
		addscore 2000
		addbonus 200
	End If
		If M9Inprogress=True And LT1.State=1 And m9=1 Then
		LT1.State=0
		LT11.State=1
		M9=M9+1
		PlaySound "sword",0,BGlevel
		Addscore 6000
		Addbonus 600
	End If 
			if M10Inprogress=True And LT1.State=1 And M10=3 Then
		LT1.State=0	
		LT9.State=1
		M10=M10+1
		PlaySound "sword",0,BGlevel
		Addscore 8000
		Addbonus 800
	End If
If M11Inprogress=True And LT1.State=1 And M11=0 Then
		LT1.State=0
		LT2.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If

End Sub


Sub Target002_hit()
		FlashEvent2
		PlaySound "metalhit_medium"
		Controller.B2SSetData 311, 0     ' DOF MX Target 2
		Controller.B2SSetData 311, 1     ' DOF MX Target 2
		If TDOF=1 then
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		End If
		AddScore 500
		Addbonus 50
		TLight002.State=1
		checkTartgetmod

	If TB=0 Then
	LJE1.State=2
	jackTimer1.Enabled=True
	End IF
	If TB=1 Then
	LJE1.State=2
	LJE2.State=2
	JackTimer1.Enabled=True
	End If
	If TB=2 Then
	LJE1.State=2
	LJE2.State=2
	LJE3.State=2
	JackTimer1.Enabled=True
	End If
	If M2Inprogress=True And LT2.State=1 Then
		LT2.State=0
		PlaySound "sword",0,BGlevel
		AddScore 2000
		Addbonus 200
		Checkmission2
	End If
	If M3Inprogress=True And LT2.State=1 Then
		LT2.State=0		
		PlaySound "sword",0,BGlevel
		Addscore 3000
		Addbonus 300
		CheckMission3
	End If
	If M4Inprogress=True And LT2.State=1 Then
		LT2.State=0		
		PlaySound "sword",0,BGlevel
		Addscore 4000
		Addbonus 400
		CheckMission4
	End If
	If M5Inprogress=True And LT2.State=1 Then
		LT2.State=0		
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT2.State=1 Then
		LT2.State=0		
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
	If M8Inprogress=True And LT2.State=1 and M8=0 Then
		LT2.State=0
		M8=M8+1
		LT4.State=1
		PlaySound "sword",0,BGlevel
		addscore 4000
		addbonus 400
	End If
		If M9Inprogress=True And LT2.State=1 And m9=3 Then
		LT2.State=0
		LT10.State=1
		M9=M9+1
		PlaySound "sword",0,BGlevel
		Addscore 6000
		Addbonus 600
	End If 
	If M10Inprogress=True And LT2.State=1 And M10=0 Then
		LT2.State=0	
		LT4.State=1
		M10=M10+1
		PlaySound "sword",0,BGlevel
		Addscore 8000
		Addbonus 800
	End If
	If M11Inprogress=True And LT2.State=1 And M11=1 Then
		LT2.State=0
		LT3.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If
End Sub

Sub Target003_hit()
		FlashEvent2
		PlaySound "metalhit_medium"
		Controller.B2SSetData 312, 1     ' DOF MX Target 3
		Controller.B2SSetData 312, 0     ' DOF MX Target 3
		If TDOF=1 then
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		End If
		AddScore 500
		Addbonus 50
		TLight003.State=1
		checkTartgetmod

	If TB=0 Then
	LJE1.State=2
	jackTimer1.Enabled=True
	End IF
	If TB=1 Then
	LJE1.State=2
	LJE2.State=2
	JackTimer1.Enabled=True
	End If
	If TB=2 Then
	LJE1.State=2
	LJE2.State=2
	LJE3.State=2
	JackTimer1.Enabled=True
	End If

	If M1Inprogress=True And LT5.State=1 Then
		LT5.State=0
		PlaySound "sword",0,BGlevel
		addscore 1000
		addbonus 100
		Checkmission1
	End If
	If M5Inprogress=True And LT5.State=1 Then
		LT5.State=0
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT5.State=1 Then
		LT5.State=0
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
	If M7Inprogress=True And LT5.State=1 And M7=1 Then
		LT5.State=0
		LT8.State=1
		M7=M7+1
		PlaySound "sword",0,BGlevel
		addscore 2000
		addbonus 200
	End If
		If M11Inprogress=True And LT5.State=1 And M11=4 Then
		LT5.State=0
		LT6.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If
End Sub

Sub Target004_hit()
		FlashEvent2
		PlaySound "metalhit_medium"
		Controller.B2SSetData 313, 1     ' DOF MX Target 4
		Controller.B2SSetData 313, 0     ' DOF MX Target 4
		If TDOF=1 then
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		End If
		AddScore 500
		Addbonus 50
		TLight004.State=1
		checkTartgetmod

	If TB=0 Then
	LJE1.State=2
	jackTimer1.Enabled=True
	End IF
	If TB=1 Then
	LJE1.State=2
	LJE2.State=2
	JackTimer1.Enabled=True
	End If
	If TB=2 Then
	LJE1.State=2
	LJE2.State=2
	LJE3.State=2
	JackTimer1.Enabled=True
	End If

	If M2Inprogress=True And LT6.State=1 Then
		LT6.State=0
		PlaySound "sword",0,BGlevel
		AddScore 2000
		Addbonus 200
		Checkmission2
	End If
	If M5Inprogress=True And LT6.State=1 Then
		LT6.State=0
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT6.State=1 Then
		LT6.State=0
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
		If M8Inprogress=True And LT6.State=1 and M8=2 Then
		LT6.State=0
		M8=M8+1
		LT11.State=1
		PlaySound "sword",0,BGlevel
		addscore 4000
		addbonus 400
	End If
	If M11Inprogress=True And LT6.State=1 And M11=5 Then
		LT6.State=0
		LT8.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If
End Sub


Sub Target005_hit()
		FlashEvent2
		Controller.B2SSetData 314, 1     ' DOF MX Target 5
		Controller.B2SSetData 314, 0     ' DOF MX Target 5
		If TDOF=1 then
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		End If
		PlaySound "metalhit_medium"
		AddScore 500
		Addbonus 50
		TLight005.State=1
		checkTartgetmod

	If TB=0 Then
	LJE1.State=2
	jackTimer1.Enabled=True
	End IF
	If TB=1 Then
	LJE1.State=2
	LJE2.State=2
	JackTimer1.Enabled=True
	End If
	If TB=2 Then
	LJE1.State=2
	LJE2.State=2
	LJE3.State=2
	JackTimer1.Enabled=True
	End If

	If M1Inprogress=True And lt8.State=1 Then
		LT8.State=0
		PlaySound "sword",0,BGlevel
		addscore 1000
		addbonus 100
		Checkmission1
	End If
	If M5Inprogress=True And LT8.State=1 Then
		LT8.State=0
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT8.State=1 Then
		LT8.State=0
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
	If M7Inprogress=True And LT8.State=1 And M7=2 Then
		LT8.State=0
		LT10.State=1
		PlaySound "sword",0,BGlevel
		addscore 2000
		addbonus 200
		M7=M7+1
	End If
		If M11Inprogress=True And LT8.State=1 And M11=6 Then
		LT8.State=0
		LT9.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If
End Sub


Sub Target006_hit()
		FlashEvent2
		PlaySound "metalhit_medium"
		Controller.B2SSetData 315, 1     ' DOF MX Target 6
		Controller.B2SSetData 315, 0     ' DOF MX Target 6
		If TDOF=1 then
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		End If
		AddScore 500
		Addbonus 50
		TLight006.State=1
		checkTartgetmod

	If TB=0 Then
	LJE1.State=2
	jackTimer1.Enabled=True
	End IF
	If TB=1 Then
	LJE1.State=2
	LJE2.State=2
	JackTimer1.Enabled=True
	End If
	If TB=2 Then
	LJE1.State=2
	LJE2.State=2
	LJE3.State=2
	JackTimer1.Enabled=True
	End If

	If M3Inprogress=True And LT9.State=1 Then
		LT9.State=0
		PlaySound "sword",0,BGlevel
		Addscore 3000
		Addbonus 300
		CheckMission3
	End If 

	If M4Inprogress=True And LT9.State=1 Then
		LT9.State=0
		PlaySound "sword",0,BGlevel
		Addscore 4000
		Addbonus 400
		CheckMission4
	End If
	If M5Inprogress=True And LT9.State=1 Then
		LT9.State=0
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT9.State=1 Then
		LT9.State=0
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
	If M9Inprogress=True And LT9.State=1  Then
		LT9.State=0
		LT1.State=1
		M9=M9+1
		PlaySound "sword",0,BGlevel
		Addscore 6000
		Addbonus 600
	End If 
	if M10Inprogress=True And LT9.State=1 And M10=4 Then
		LT9.State=0	
		LT3.State=1
		M10=M10+1
		PlaySound "sword",0,BGlevel
		Addscore 8000
		Addbonus 800
	End If
	If M11Inprogress=True And LT9.State=1 And M11=7 Then
		LT9.State=0
		LT10.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If
End Sub

Sub Target007_hit()
		FlashEvent2
		PlaySound "metalhit_medium"
		Controller.B2SSetData 316, 1     ' DOF MX Target 7
		Controller.B2SSetData 316, 0     ' DOF MX Target 7
		If TDOF=1 then
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		End If
		AddScore 500
		Addbonus 50
		TLight007.State=1
		checkTartgetmod

	If TB=0 Then
	LJE1.State=2
	jackTimer1.Enabled=True
	End IF
	If TB=1 Then
	LJE1.State=2
	LJE2.State=2
	JackTimer1.Enabled=True
	End If
	If TB=2 Then
	LJE1.State=2
	LJE2.State=2
	LJE3.State=2
	JackTimer1.Enabled=True
	End If

	If M1Inprogress=True And LT10.State=1 Then
		LT10.State=0
		PlaySound "sword",0,BGlevel
		addscore 1000
		addbonus 100
		Checkmission1
	End If
	If M3Inprogress=True And LT10.State=1 Then
		LT10.State=0
		PlaySound "sword",0,BGlevel
		Addscore 3000
		Addbonus 300
		CheckMission3
	End If
	If M4Inprogress=True And LT10.State=1 Then
		LT10.State=0
		PlaySound "sword",0,BGlevel
		Addscore 4000
		Addbonus 400
		CheckMission4
	End If
	If M5Inprogress=True And LT10.State=1 Then
		LT10.State=0
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT10.State=1 Then
		LT10.State=0
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
		If M7Inprogress=True And LT10.State=1 And M7=3 Then
		LT10.State=0
		M7=M7+1
		PlaySound "sword",0,BGlevel
		addscore 1000
		addbonus 100
		Checkmission7
	End If
		If M9Inprogress=True And LT10.State=1 And m9=4 Then
		LT10.State=0
		LT3.State=1
		M9=M9+1
		PlaySound "sword",0,BGlevel
		Addscore 6000
		Addbonus 600
	End If 
			if M10Inprogress=True And LT10.State=1 And M10=2 Then
		LT10.State=0	
		LT1.State=1
		M10=M10+1
		PlaySound "sword",0,BGlevel
		Addscore 8000
		Addbonus 800
	End If
	If M11Inprogress=True And LT10.State=1 And M11=8 Then
		LT10.State=0
		LT11.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If
End Sub

Sub Target008_hit()
		FlashEvent2
		PlaySound "metalhit_medium"
		Controller.B2SSetData 317, 1     ' DOF MX Target 8
		Controller.B2SSetData 317, 0     ' DOF MX Target 8
		If TDOF=1 then
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		End If
		AddScore 500
		Addbonus 50
		Tlight008.State=1
		checkTartgetmod

	If TB=0 Then
	LJE1.State=2
	jackTimer1.Enabled=True
	End IF
	If TB=1 Then
	LJE1.State=2
	LJE2.State=2
	JackTimer1.Enabled=True
	End If
	If TB=2 Then
	LJE1.State=2
	LJE2.State=2
	LJE3.State=2
	JackTimer1.Enabled=True
	End If

	If M2Inprogress=True And LT11.State=1 Then
		LT11.State=0
		PlaySound "sword",0,BGlevel
		AddScore 2000
		Addbonus 200
		Checkmission2
	End If
	If M3Inprogress=True And LT11.State=1 Then
		LT11.State=0
		PlaySound "sword",0,BGlevel
		Addscore 3000
		Addbonus 300
		CheckMission3
	End If
	If M5Inprogress=True And LT11.State=1 Then
		LT11.State=0
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT11.State=1 Then
		LT11.State=0
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
		If M8Inprogress=True And LT11.State=1 and M8=3 Then
		LT11.State=0
		M8=M8+1
		PlaySound "sword",0,BGlevel
		addscore 4000
		addbonus 400
		Checkmission8
	End If
	If M9Inprogress=True And LT11.State=1 And m9=2 Then
		LT11.State=0
		LT2.State=1
		M9=M9+1
		PlaySound "sword",0,BGlevel
		Addscore 6000
		Addbonus 600
	End If 
	If M11Inprogress=True And LT11.State=1 And M11=9 Then
		LT11.State=0
		M11=M11+1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
	Checkmission11


	End If
End Sub


Dim TB

Sub checkTartgetmod()
	If TLight001.State=1 And TLight002.State=1 And  TLight003.State=1 And TLight004.State=1 And TLight005.State=1 And TLight006.State=1 And  TLight007.State=1 And TLight008.State=1  And TB=0 Then
		TB=TB+1
		TLight001.State=0
		TLight002.State=0
		TLight003.State=0
		TLight004.State=0
		TLight005.State=0
		TLight006.State=0
		TLight007.State=0
		TLight008.State=0
		Addscore 25000
		Addbonus 2500
If Voice =1 Or Voice=2 Then playsound "JackF",0,BGlevel End If
If Voice =0 Then playsound "JackM",0,BGlevel End If
		UltraDMD.DisplayScene00 "JJackpot.gif", "", 0, "", 0, 14, GifTime1, 14
		LightSeqRamp.Play SeqLeftOn, 10
		Flashevent3
		Jacklight.State=1
		Controller.B2SSetData 360, 1	' DOF MX Jackpot
		Controller.B2SSetData 360, 0	' DOF MX Jackpot
		
	End If
	If TLight001.State=1 And TLight002.State=1 And  TLight003.State=1 And TLight004.State=1 And TLight005.State=1 And TLight006.State=1 And  TLight007.State=1 And TLight008.State=1  And TB=1 Then
		TB=TB+1
		TLight001.State=0
		TLight002.State=0
		TLight003.State=0
		TLight004.State=0
		TLight005.State=0
		TLight006.State=0
		TLight007.State=0
		TLight008.State=0
		Addscore 50000
		Addbonus 5000
If Voice =1 Then playsound "DJackF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "DJackM",0,BGlevel End If
		UltraDMD.DisplayScene00 "JDoubleJackpot.gif", "", 0, "", 0, 14, GifTime1, 14
		LightSeqRamp.Play SeqLeftOn, 10
		Flashevent3
		DoubleJacklight.State=1	
		Controller.B2SSetData 360, 1	' DOF MX Jackpot
		Controller.B2SSetData 360, 0	' DOF MX Jackpot	


	End If
	If TLight001.State=1 And TLight002.State=1 And  TLight003.State=1 And TLight004.State=1 And TLight005.State=1 And TLight006.State=1 And  TLight007.State=1 And TLight008.State=1  And TB=2 Then
		TB=TB+1
		TLight001.State=0
		TLight002.State=0
		TLight003.State=0
		TLight004.State=0
		TLight005.State=0
		TLight006.State=0
		TLight007.State=0
		TLight008.State=0
		Addscore 100000
		Addbonus 10000
If Voice =1 Or Voice=2 Then playsound "SJacktF",0,BGlevel End If
If Voice =0 Then playsound "SjackM",0,BGlevel End If
		UltraDMD.DisplayScene00 "JSuperjackpot.gif", "", 0, "", 0, 14, GifTime1, 14
		LightSeqRamp.Play SeqLeftOn, 10
		Flashevent3
		Superjacklight.State=1
		Controller.B2SSetData 360, 1	' DOF MX Jackpot
		Controller.B2SSetData 360, 0	' DOF MX Jackpot
	End If
	If TLight001.State=1 And TLight002.State=1 And  TLight003.State=1 And TLight004.State=1 And TLight005.State=1 And TLight006.State=1 And  TLight007.State=1 And TLight008.State=1 And TB=3 Then
		TB=0
		TLight001.State=0
		TLight002.State=0
		TLight003.State=0
		TLight004.State=0
		TLight005.State=0
		TLight006.State=0
		TLight007.State=0
		TLight008.State=0
		Addscore 500000
		Addbonus 50000
If Voice =1 Then playsound "DSJackF",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "DSJackM",0,BGlevel End If
		UltraDMD.DisplayScene00 "JSuperDoubleJackpot.gif", "", 0, "", 0, 14, GifTime1, 14
		Flashevent3
		LightSeqRamp.Play SeqLeftOn, 10
		DoubleJacklight.State=1
		Superjacklight.State=1
		JackTimer2.Enabled=False
		Controller.B2SSetData 360, 1	' DOF MX Jackpot
		Controller.B2SSetData 360, 0	' DOF MX Jackpot
	End If
End Sub



Sub Triggerspin_hit()
	If M3Inprogress=True And LT3.State=1 Then
		LT3.State=0
		PlaySound "sword",0,BGlevel
		AddScore 3000
		Addbonus 300
		CheckMission3
	End If
	If M4Inprogress=True And LT3.State=1 Then
		LT3.State=0
		PlaySound "sword",0,BGlevel
		AddScore 4000
		Addbonus 400
		CheckMission4
	End If
	If M5Inprogress=True And LT3.State=1 Then
		LT3.State=0
		PlaySound "sword",0,BGlevel
		addscore 5000
		addbonus 500
		Checkmission5
	End If
	If M6Inprogress=True And LT3.State=1 Then
		LT3.State=0
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		CheckMission6
	End If
		If M9Inprogress=True And LT3.State=1 And m9=5 Then
		LT3.State=0
		M9=M9+1
		PlaySound "sword",0,BGlevel
		Addscore 6000
		Addbonus 600
		CheckMission9
	End If 
		if M10Inprogress=True And LT3.State=1 Then
		LT3.State=0	
		M10=M10+1
		PlaySound "sword",0,BGlevel
		Addscore 8000
		Addbonus 800
		CheckMission10
	End If
	If M11Inprogress=True And LT3.State=1 And M11=2 Then
		LT3.State=0
		LT4.State=1
		PlaySound "sword",0,BGlevel
		addscore 10000
		addbonus 1000
		M11=M11+1
	End If
End Sub

'***********MULTIBALLLOCK*****************
Dim Lock1

Sub Targetlocl1_hit()
		FlashDrop
		PlaySound "fx_droptarget"
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		Controller.B2SSetData 318, 1    ' DOF MX Target Lock 1
		Controller.B2SSetData 318, 0    ' DOF MX Target Lock 1
	If MissionInProgress=False And MULTILOCK=False Then
		Targetlocl1.isDropped=True
		Addscore 1000
		AddBonus 100
	End If
	If MULTILOCK=True Then
		TargetTimer.Enabled=True
	End If
End Sub

Sub Targetlock2_hit()
		FlashDrop
		PlaySound "fx_droptarget"
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		Controller.B2SSetData 319, 1    ' DOF MX Target Lock 2
		Controller.B2SSetData 319, 0    ' DOF MX Target Lock 2
		If MULTILOCK=False Then
		Targetlock2.isDropped=True
		Addscore 1000
		AddBonus 100
	End If
End Sub
Dim KickblockIP : KickblockIP=False
Sub Targetlock3_hit
		FlashDrop
		PlaySound "fx_droptarget"
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		Controller.B2SSetData 320, 1    ' DOF MX Target Lock 3
		Controller.B2SSetData 320, 0    ' DOF MX Target Lock 3
	If MULTILOCK=False Then
		Targetlock3.isDropped=True
		Addscore 1000
		AddBonus 100
		If Lock1=0 Then
		KickblockIp=true
		End If
	End If
End Sub



Sub TargetTimer_Timer()
		TargetTimer.Enabled =False
		Targetlocl1.IsDropped=False
		PlaySound "metalhit_medium"
End Sub

Sub ResetTargets()
		Targetlocl1.IsDropped = false 'Target A pops up to maximum height
		Targetlock2.IsDropped = false 'Target B pops up to maximum height
		Targetlock3.IsDropped = false 'Target C pops up to maximum height
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		Controller.B2SSetData 321, 1     ' DOF MX Reset Targets
		Controller.B2SSetData 321, 0     ' DOF MX Reset Targets
End Sub

Sub KickerLock_hit()
		LockFlash
		Addscore 10000
		Addbonus 1000
	If MULTILOCK=True Then
Timerlockon.Enabled=True
	End If
If Lock1=0 Then
		Controller.B2SSetData 365, 1     ' DOF MX Lock 1
		Controller.B2SSetData 365, 0     ' DOF MX Lock 1
		Locked1Timer.Enabled=True
		Balllock2.Enabled=False
		Timer001.Enabled=True	
    UltraDMD.DisplayScene00 "ball1lock.gif", "", 0, "", 0, 14, GifTime1, 14
If Voice =1 Then playsound "Ball1F",0,BGlevel End If
If Voice =0 Or Voice=2 Then playsound "Ball1M",0,BGlevel End If
	End If
	IF Lock1=1 Then
	Balllock2.Enabled=True
	Locked2Timer.Enabled=True
		Controller.B2SSetData 366, 1     ' DOF MX Lock 2
		Controller.B2SSetData 366, 0     ' DOF MX Lock 2
	Timer001.Enabled=True	
    UltraDMD.DisplayScene00 "ball2lock.gif", "", 0, "", 0, 14, GifTime1, 14
If Voice =1 Or Voice=2 Then playsound "Ball2F",0,BGlevel End If
If Voice =0 Then playsound "Ball2M",0,BGlevel End If
	End If
	If Lock1=2 Then
	locktimer.Enabled=true
	TimerFlash.Enabled=True
	LightSeqAttract.Play SeqRandom,150,,4000
	Ballinpf=ballinpf+2
    UltraDMD.DisplayScene00 "Multiball.gif", "", 0, "", 0, 14, GifTime1, 14
If Voice =1 Then playsound "MultiF",0,BGlevel End If
If Voice =0 Or Voice=2  Then playsound "MultiM",0,BGlevel End If
	Multijacklight.State=1
	lock1=lock1+1
	TBallsave.Enabled=False
	EndMusic
	Playmusic "DPMulti.mp3", 0.5
		Controller.B2SSetData 367, 1     ' DOF MX Multiball
		Controller.B2SSetData 367, 0     ' DOF MX Multiball
	End If
End Sub

Sub locktimer_Timer()
		Locktimer.Enabled=False
		Balllock1.kick 0,50
		Balllock2.Kick 0,20
		MultiTime.Enabled=True
		locklight3.State=1
		playSound "ballrelease"
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		Controller.B2SSetData 132, 1
		Controller.B2SSetData 132, 0
	End Sub
		

Sub Locked1Timer_timer
	Locked1Timer.Enabled=False
	KickerLock.Kick 0,30
	kicker1.Createball
	kicker1.Kick 90,7
	Plunger.PullBack
	Plungertime.Enabled=True
	Lock1=Lock1+1
	locklight1.State=1
	Locklight2.State=2
	playSound "ballrelease"
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		Controller.B2SSetData 132, 1
		Controller.B2SSetData 132, 0
	End Sub

Sub Locked2Timer_timer
	Locked2Timer.Enabled=False
	Balllock2.Kick 0,10
	timer002.Enabled=True
	kicker1.Createball
	kicker1.Kick 90,7
	Plunger.PullBack
	Plungertime.Enabled=True
	Lock1=Lock1+1
	locklight2.State=1
	LockLight3.State=2

		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		Controller.B2SSetData 132, 1
		Controller.B2SSetData 132, 0
End Sub
Sub Timer002_Timer
timer002.Enabled=False
KickerLock.kick 0,30
	playSound "ballrelease"
End Sub

Sub TimerLockon_Timer
Timerlockon.Enabled=False
KickerLock.kick 180,10
End Sub


Sub MultiTime_timer
	Multitime.Enabled=False
	TimerFlash.Enabled=false
	Balllock1.Kick 0,50
	Balllock2.Kick 0, 20
	lockEject.Enabled=True
	KickerLock.kick 180, 20
	LightSeqAttract.StopPlay
	locklight1.State=2
	Locklight2.State=2
	Locklight3.State=2
	Targetlocl1.IsDropped = True 'Target A pops up to maximum height
	Targetlock2.IsDropped = True 'Target B pops up to maximum height
	Targetlock3.IsDropped = True 'Target C pops up to maximum height
	MULTILOCK=True
	playSound "ballrelease"
		Controller.B2SSetData 110, 1
		Controller.B2SSetData 110, 0
		Controller.B2SSetData 132, 1
		Controller.B2SSetData 132, 0
		Tballsave2.Enabled=True
		BallsaveLight.State=1
	End Sub
Sub Timer001_Timer
Timer001.Enabled=false	
ResetTargets
End Sub


Sub Lockeject_Timer
	Balllock1.Kick 0,25
playSound "ballrelease"
	lockEject.Enabled=False
End Sub
	
Sub Plungertime_timer
	Plungertime.Enabled=False
	Plunger.Fire 
	End Sub
 
Sub Spinner001_Spin()
		PlaySound "target"
		Addscore 100
		Controller.B2SSetData 332, 2    ' DOF MX Spinner
		Controller.B2SSetData 332, 0    ' DOF MX Spinner
End Sub
'****************KICKBACK***************
Dim KB

Sub kicker001_hit
	If Klight.State=1 then
		kicker001.kick 356,55
		PlaySound "ballrelease"
		Controller.B2SSetData 103, 1
		Controller.B2SSetData 103, 0
		Controller.B2SSetData 333, 1	' DOF MX Kback
		Controller.B2SSetData 333, 0	' DOF MX Kback
		klight.State=0
		kickbackinprogress=False
		'PlaySound "KickBack", 2
		Klight001.State=1
		Klight002.State=1
	End If
	If Klight.State=0 Then
		Kicker001.Kick 90,5
	End If
End Sub

Sub TkickBacktrig_Timer()
		TkickBacktrig.Enabled=False
	If Kickbackinprogress=true Then
		Klight001.State=0
		Klight002.State=0
	End If
	If kickbackinprogress=False Then
		Klight001.State=1
		Klight002.State=1
		Tkickbacktrig.Enabled=True
	End If

End Sub


Sub Triggerkickback1_hit
If TkickBacktrig.Enabled=False And Kickbackinprogress=False Then
TkickBacktrig.Enabled=True
Klight001.State=0
End If
If TkickBacktrig.Enabled=True And Klight.State=0 And Klight001.State=1 Then
Klight001.State=0
End If
CheckKickBack
End Sub

Sub triggerkickback2_hit
If TkickBacktrig.Enabled=False And Kickbackinprogress=False Then
TkickBacktrig.Enabled=True
Klight002.State=0
End If
If TkickBacktrig.Enabled=true And Klight.State=0 And Klight002.State=1 Then
Klight002.State=0
End If
CheckKickBack
End Sub

Sub CheckKickBack
If Klight001.State=0 and Klight002.State=0 And Tkickbacktrig.Enabled=True And Klight.State=0 Then
Klight.State=1
klight001.State=0
klight002.State=0
Kickbackinprogress=True
If Voice =1 Or Voice=2 Then playsound "kickF",0,BGlevel End If
If Voice =0 Then playsound "kickM",0,BGlevel End If
End If



End Sub

Sub Trigger001_hit
If Moveit=1 Then : Princess3d.ObjRotZ = 4 : End If
TimerTrig.Enabled=True
triggerkickback2.Enabled=False
End Sub

Sub TimerTrig_Timer
TimerTrig.Enabled=False
triggerkickback2.Enabled=True
End Sub
'*************************************************** ---- Flashers Begin
Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4
FlasherLight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
Flasherlight4.IntensityScale = 0


'*** white flasher ***
Sub FlasherFlash1_Timer()
	dim flashx3, matdim
	If not FlasherFlash1.TimerEnabled Then 
		FlasherFlash1.TimerEnabled = True
		FlasherFlash1.visible = 1
		FlasherLit1.visible = 1
	End If
	flashx3 = FlashLevel1 * FlashLevel1 * FlashLevel1
	Flasherflash1.opacity = 1000 * flashx3
	FlasherLit1.BlendDisableLighting = 10 * flashx3
	Flasherbase1.BlendDisableLighting =  flashx3
	FlasherLight1.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel1)
	FlasherLit1.material = "domelit" & matdim
	FlashLevel1 = FlashLevel1 * 0.85 - 0.01
	If FlashLevel1 < 0.15 Then
		FlasherLit1.visible = 0
	Else
		FlasherLit1.visible = 1
		Controller.B2SSetData 380, 1  ' DOF MX White Flasher
	end If
	If FlashLevel1 < 0 Then
		FlasherFlash1.TimerEnabled = False
		FlasherFlash1.visible = 0
		Controller.B2SSetData 380, 0  ' DOF MX White Flasher
	End If
End Sub

'*** Red flasher ***
Sub FlasherFlash2_Timer()
	dim flashx3, matdim
	If not Flasherflash2.TimerEnabled Then 
		Flasherflash2.TimerEnabled = True
		Flasherflash2.visible = 1
		Flasherlit2.visible = 1
	End If
	flashx3 = FlashLevel2 * FlashLevel2 * FlashLevel2
	Flasherflash2.opacity = 1500 * flashx3
	Flasherlit2.BlendDisableLighting = 10 * flashx3
	Flasherbase2.BlendDisableLighting =  flashx3
	Flasherlight2.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel2)
	Flasherlit2.material = "domelit" & matdim
	FlashLevel2 = FlashLevel2 * 0.9 - 0.01
	If FlashLevel2 < 0.15 Then
		Flasherlit2.visible = 0
	Else
		Flasherlit2.visible = 1
		Controller.B2SSetData 381, 1  ' DOF MX Red Flasher
	end If
	If FlashLevel2 < 0 Then
		Flasherflash2.TimerEnabled = False
		Flasherflash2.visible = 0
		Controller.B2SSetData 381, 0  ' DOF MX Red Flasher
	End If
End Sub

'*** blue flasher vertical ***
Sub FlasherFlash3_Timer()
	dim flashx3, matdim
	If not Flasherflash3.TimerEnabled Then 
		Flasherflash3.TimerEnabled = True
		Flasherflash3.visible = 1
		Flasherlit3.visible = 1
	End If
	flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
	Flasherflash3.opacity = 8000 * flashx3
	Flasherlit3.BlendDisableLighting = 10 * flashx3
	Flasherbase3.BlendDisableLighting =  flashx3
	Flasherlight3.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	Flasherlit3.material = "domelit" & matdim
	FlashLevel3 = FlashLevel3 * 0.85 - 0.01
	If FlashLevel3 < 0.15 Then
		Flasherlit3.visible = 0
	Else
		Flasherlit3.visible = 1
		Controller.B2SSetData 382, 1  ' DOF MX Blue Flasher 1
	end If
	If FlashLevel3 < 0 Then
		Flasherflash3.TimerEnabled = False
		Flasherflash3.visible = 0
		Controller.B2SSetData 382, 0  ' DOF MX Blue Flasher 1
	End If
End Sub

'*** blue flasher vertical ***
Sub FlasherFlash4_Timer()
	dim flashx3, matdim
	If not Flasherflash4.TimerEnabled Then 
		Flasherflash4.TimerEnabled = True
		Flasherflash4.visible = 1
		Flasherlit4.visible = 1
	End If
	flashx3 = FlashLevel4 * FlashLevel4 * FlashLevel4
	Flasherflash4.opacity = 8000 * flashx3
	Flasherlit4.BlendDisableLighting = 10 * flashx3
	Flasherbase4.BlendDisableLighting =  flashx3
	Flasherlight4.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel4)
	Flasherlit4.material = "domelit" & matdim
	FlashLevel4 = FlashLevel4 * 0.85 - 0.01
	If FlashLevel4 < 0.15 Then
		Flasherlit4.visible = 0
	Else
		Flasherlit4.visible = 1
		Controller.B2SSetData 383, 1  ' DOF MX Blue Flasher 2
	end If
	If FlashLevel4 < 0 Then
		Flasherflash4.TimerEnabled = False
		Flasherflash4.visible = 0
		Controller.B2SSetData 383, 0  ' DOF MX Blue Flasher 2
	End If
End Sub
'*************************************************** ---- Flashers End

'*** -------------------- script only for demoing flashers ***
dim countr

Sub TimerFlash_Timer
	countr = countr + 1
	If Countr > 3 then Countr = 0 end If
	If rnd(1) < 0.5 Then 
		select case countr
			case 0 : FlashLevel1 = 2 : FlasherFlash1_Timer
			case 1 : FlashLevel2 = 2 : FlasherFlash2_Timer
			case 2 : FlashLevel3 = 2 : FlasherFlash3_Timer
			case 3 : FlashLevel4 = 2 : FlasherFlash4_Timer
		end Select
	End If
end Sub
'*** -------------------- END script only for demoing flashers ***
Dim fs

Sub Flashevent
FlashLevel3 = 2 : FlasherFlash3_Timer
FlashLevel4 = 2 : FlasherFlash4_Timer
End Sub

Sub FlashEvent2
FlashLevel1 = 1 : FlasherFlash1_Timer
FlashLevel2 = 1 : FlasherFlash2_Timer
End Sub

Sub FlashDrop
FlashLevel2 = 2 : FlasherFlash2_Timer
End Sub

Sub LockFlash
		FlashLevel1 = 4 : FlasherFlash1_Timer
		FlashLevel2 = 4 : FlasherFlash2_Timer
		FlashLevel3 = 4 : FlasherFlash3_Timer
		FlashLevel4 = 4 : FlasherFlash4_Timer
End Sub

Sub Flashevent3

		FlashLevel1 = 1 : FlasherFlash1_Timer
		FlashLevel2 = 1 : FlasherFlash2_Timer
		FlashLevel3 = 1 : FlasherFlash3_Timer
		FlashLevel4 = 1 : FlasherFlash4_Timer
		TimerTemp.Enabled=True

End Sub

Sub TimerTemp_Timer

		FlashLevel1 = 4 : FlasherFlash1_Timer
		FlashLevel2 = 4 : FlasherFlash2_Timer
		FlashLevel3 = 4 : FlasherFlash3_Timer
		FlashLevel4 = 4 : FlasherFlash4_Timer
		TimerTemp.Enabled=False 
End Sub


Sub FlashMission
		FlashLevel1 = 4 : FlasherFlash1_Timer
		FlashLevel2 = 4 : FlasherFlash2_Timer
		FlashLevel3 = 4 : FlasherFlash3_Timer
		FlashLevel4 = 4 : FlasherFlash4_Timer
		TFlashM.Enabled=True
End Sub
Dim FM
Sub TFlashM_Timer
		FM=FM+1
		FlashLevel1 = 4 : FlasherFlash1_Timer
		FlashLevel2 = 4 : FlasherFlash2_Timer
		FlashLevel3 = 4 : FlasherFlash3_Timer
		FlashLevel4 = 4 : FlasherFlash4_Timer
		TFlashM.Enabled=False
If FM<5 Then TFlashM.Enabled=True End If
If FM=5 Then Exit Sub End If
End Sub 

'---------Princess 3D Motion on Ramp with Gear Dof--------
dim RampMove : RampMove = 0
dim positt:positt= 0
dim stopitt:stopitt = 0
	Sub PrincTm_timer()	
		If Princess3d.ObjRotZ = 0 Then
			positt = 0
			stopitt = stopitt+1
		end If
		If stopitt > 3 Then PrincTm.enabled = 0 : RampMove = 0 : Exit Sub
		If Princess3d.ObjRotZ = 20 Then
			positt = 1
		End If
		If positt = 0 Then
			Princess3d.ObjRotZ = Princess3d.ObjRotZ + 4
		Else
			Princess3d.ObjRotZ = Princess3d.ObjRotZ - 4
		end If
	end Sub

	sub MovePrinc
	PrincTm.enabled = 1
	Controller.B2SSetData 132, 1
	Controller.B2SSetData 132, 0
	end Sub

	Sub RampStart_Hit
		if Moveit=1 then
			if RampMove < 1 then
			RampMove = 1
			MovePrinc
			stopitt = 0
		Controller.B2SSetData 329, 1    ' DOF MX Ramp Navigate
		Controller.B2SSetData 329, 0    ' DOF MX Ramp Navigate		
			end if
		end if
	End Sub
'---------------------------------------------------------

Dim Princess3dStep
Sub Timer8_Timer

	Princess3dStep = Princess3dStep + 1

	Select Case Princess3dStep
		Case 1:
			Princess3d.ObjRotZ = 4
		Case 2:
			Princess3d.ObjRotZ = 12
		Case 3:
			Princess3d.ObjRotZ = -4
		Case 4:
			Princess3d.ObjRotZ = 12
		Case 5:
			Princess3d.ObjRotZ = -4
		Case 6:
			Princess3d.ObjRotZ = 12
		Case 7:
			Princess3d.ObjRotZ = -4
		Case 8:
			Princess3d.ObjRotZ = 12
		Case 9:
			Princess3d.ObjRotZ = -4
		Case 10:
			Timer8.Enabled=False


			Princess3dStep = 1
	End Select
End Sub

'******* Some Sound FX / SSF *******

Sub rRubberBand002_hit
PlaySound "fx_rubber_hit_1",0,0.8,0,0.25
End Sub
Sub rRubberBand004_hit
PlaySound "fx_rubber_hit_2",0,0.8,0,0.25
End Sub
Sub rRubberBand006_hit
PlaySound "fx_rubber_hit_3",0,0.8,0,0.25
End Sub
Sub rRubberBand11_hit
PlaySound "fx_rubber_hit_1",0,0.8,0,0.25
End Sub
Sub Pin001_hit
PlaySound "fx_rubber_hit_3",0,0.8,-1.0,1.0,1,1
End Sub
Sub Pin3_hit
PlaySound "fx_rubber_hit_3",0,0.8,-1.0,1.0,1,1
End Sub
Sub Pin4_hit
PlaySound "fx_rubber_hit_3",0,0.8,1.0,1.0,1,1
End Sub
Sub RubberBand001_hit
PlaySound "fx_rubber_hit_3",0,0.8,-1.0,1.0,1,1
End Sub
Sub RubberBand13_hit
PlaySound "fx_rubber_hit_3",0,0.8,1.0,1.0,1,1
End Sub
Sub pRubberPostY006_hit
PlaySound "fx_rubber_hit_3",0,0.8,-1.0,1.0,1,1
End Sub
Sub pRubberPostY005_hit
PlaySound "fx_rubber_hit_3",0,0.8,-1.0,1.0,1,1
End Sub
Sub pRubberPostY002_hit
PlaySound "fx_rubber_hit_3",0,0.8
End Sub
Sub pRubberPostY004_hit
PlaySound "fx_rubber_hit_3",0,0.8
End Sub
Sub pRubberPostY2_hit
PlaySound "fx_rubber_hit_3",0,0.8,1.0,1.0,1,1
End Sub
Sub pRubberPostY001_hit
PlaySound "fx_rubber_hit_3",0,0.8,1.0,1.0,1,1
End Sub
Sub Wall012_hit
PlaySound "fx_rubber_hit_2",0,0.5,0.0,1.0,1,1
End Sub
Sub Wall018_hit
PlaySound "fx_rubber_hit_2",0,0.5,0.0,1.0,1,1
End Sub
Sub Wall019_hit
PlaySound "fx_rubber_hit_2",0,0.5,0.0,1.0,1,1
End Sub
Sub Wall005_hit
PlaySound "fx_rubber_hit_2",0,0.5,0.0,1.0,1,1
End Sub
Sub Wall010_hit
PlaySound "fx_rubber_hit_2",0,0.5,0.0,1.0,1,1
End Sub
Sub Ramp002_hit
PlaySound "fx_metalwall",0,0.8,-1.0,1.0,1,1
End Sub
Sub Ramp021_hit
PlaySound "fx_metalwall",0,0.8,1.0,1.0,1,1
End Sub
Sub Rubber014_hit
PlaySound "fx_metalwall",0,1,1.0,1.0,1,1
End Sub
' flipper hit sounds
Sub LeftFlipper_Collide(parm)
    PlaySound "fx_flip_hit_L", 0, parm / 10, -0.9, 0.25
End Sub
Sub RightFlipper_Collide(parm)
    PlaySound "fx_flip_hit_R", 0, parm / 10, 0.9, 0.25
End Sub
