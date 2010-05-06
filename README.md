DDalyze
=======

I have an obsession with creating the perfect tower defense setups. Desktop
Defender is my personal flavor of choice for this.

I've spent a lot of time thinking about the tower defense, I wanted to see what
I could get by thinking about how to think about tower defense - using the
computer to do heavy lifting for me.

Thus DDalyze.

This is not a finished project, nor is there anything you can run at a command
line.

If you use swank-clojure or leiningen to load up a REPL, you could execute the
following command to get started:

    (set-current-map (map-from-map-file "/Users/alanshields/code/desktop_defender/maps/basic2.map"))

Then to perform an analysis with a budget of 40 gold

     (update-current-map 40)

The code in core.clj is the "interface" to underlying code.

I probably won't get a chance to play with this again for a bit, but for those who
are interested....

Alan Shields
