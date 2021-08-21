const compress_images = require("compress-images");
const myArgs = process.argv.slice(2);

const INPUT_path_to_your_images = myArgs[0];//"src/**/*.{jpg,JPG,jpeg,JPEG,png,svg,gif}";
const OUTPUT_path = myArgs[1];//"build/";

compress_images(INPUT_path_to_your_images, OUTPUT_path, { compress_force: false, statistic: true, autoupdate: true, pathLog: myArgs[2] }, false,
                { jpg: { engine: "mozjpeg", command: ["-quality", myArgs[3]] } },
                { png: { engine: "pngquant", command: [myArgs[4], "-o"] } },
                { svg: { engine: "svgo", command: "--multipass" } },
                { gif: { engine: "gifsicle", command: ["--colors", myArgs[5], "--use-col=web"] } },
  function (error, completed, statistic) {
    console.log("## Statistics");
    console.log(statistic);
    console.log("## error");
    console.log(error);
    console.log("## completed");
    console.log(completed);
  }
);
