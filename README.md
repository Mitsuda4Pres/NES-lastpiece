# NES-dod
Data-oriented entity management system on NES

Big thanks to @clearvus YouTube archive of a stream he did 7 years ago. I learn best by walking through at pace with someone, even if they make a few mistakes along the way. Perhaps because of it. Witnessing the logic of working through assembly code and not the polished final product of a tutorial was very helpful for me.
https://www.youtube.com/playlist?list=PL29OkqO3wUxyF9BsTAgZkmCEVtC77rgff

After video 5 he gets basic collision detection running, then talks about wanting to do a more data-oriented version. At this point I paused my progress because I was already wondering if there was a better data-oriented approach to his entire entity management system (which was very helpful for me to understand OOP-type structures in assembly). So with no further knowledge or research, and knowing naught but the barest concept of what data-oriented design even is, I tried my hand at a data-oriented entity management system on NES, with the aim to see how hard I can tax the machine.

Controls:
-Arrows move
-A to shoot
-B to spawn an enemy at top of screen


Anyways, we'll see if it works.

1st bug: Of course it didn't work on first load. I'm not that good. I have a pointer for loading the background that uses ptr and ptr+1, but only reserved 1 byte for it. So it was bleeding into my object counter for tracking how many entities have been created. Reserve 2 bytes should fix that problem.

2nd bug: Heck I had to work out a lot of bugs. The biggest was that I start by declaring the arrays in the PRG ROM, which definitely wasn't going to work. But I learned a lot about how the NES works because of it. So I moved the arrays to $0400 (x and y positions) and $0500 (entity type, subpixel movement counter, and health -not used-).

It still only puts up about 22 sprites before weird things start happening. NES is supposed to be capable of displaying 64, so I am probably getting some kind of memory bleed somewhere, but at the moment I'm happy to have the system working well. I have a hunch that I may need to mess with the actual memory map in the linker config to optimize.

Enjoy my YYCHR graphics I mouse-clicked together in under and hour. The forest almost tesselates, sort of!