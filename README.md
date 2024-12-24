# haskell-rpp

This project provides a library for reading Reaper RPP file as well as a few utility:

## dump

Extract informations from a project:

- [x] marker timeline
- [ ] tracks and fx
- [ ] media items

## split-video

Use this command to apply cuts to a video:

- Demux the audio from video,
- Import audio in Reaper and makes cut
- Run the tool to generate a concat script based on the cuts and execute ffmpeg to make the final video.


## Reference

https://wiki.cockos.com/wiki/index.php/State_Chunk_Definitions
