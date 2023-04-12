namespace SilverBrain

module TestApp =
    [<EntryPoint>]
    let main _args =
        Store.Migration.Runner.run false [ "/home/sheep/temp/silver-brain.dev/new.sqlite" ]
        0
