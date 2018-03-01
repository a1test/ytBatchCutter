Downloads videos, cuts each on parts and merges them. Determines length of each merged video.
Requires installed ffmpeg and youtube-dl.

Usage: ytMultiCut INPUTFILE

INPUTFILE format:
```
	[Config section]

	URL1
	[Description]
	[TimeFrom1 - TimeTo1]
	[TimeFrom2 - TimeTo2]
	[...]

	[URL2]
	...
```

Time ranges defines parts that should be cut from original video and then merged. Merged videos are created in folder Concatenated that created in same folder as INPUTFILE located. 
Order of URL, Description and time ranges can be arbitrary, but different video sections must be separated by one or more empty lines.
Each time range must be separated by character "-". It can be surrounded by spaces.
Time values are passed as it is to ffmpeg as `-ss TimeFrom1` and `-t TimeFrom2` parameters and must be supported by ffmpeg.
INPUTFILE support # as comment line only at line start.

Example of file:
```
	https://www.youtube.com/watch?v=dQw4w9WgXcQ
	0 - 0:10
	1:12 - 3:40
	Comment about video

	0:16 - 1:07
	https://www.youtube.com/watch?v=FPRCppkU6XU
```
Config section format:
```
	concat-all=Final video name
	youtube-dl=youtube-dl.exe [download parameters]
	ffmpeg-cut=ffmpeg.exe [each timerange processing parameters template] 
	ffmpeg-concat=ffmpeg.exe [each video concatenating parameters template]
```

ffmpeg and youtube-dl parameters should contain path to ffmpeg.exe and youtube-dl.exe, respectively, if they are not specified in %PATH% variable.
If some parameter is not specified default config options will be used.
If concat-all parameter is not specified all videos will not be merged.
Empty parameter value will lead to skipping corresponding actiong.