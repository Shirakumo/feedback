class Feedback{
    constructor(){
        var edit = document.querySelector(".project.edit");
        if(edit){
            var template = edit.querySelector(".attachment").cloneNode(true);
            edit.querySelector(".button.add").addEventListener("click", (e)=>{
                var copy = template.cloneNode(true);
                copy.querySelector(".button.delete").addEventListener("click", (e)=>copy.remove());
                edit.querySelector(".attachments ul").appendChild(copy);
            });
            [].forEach.call(edit.querySelectorAll(".attachment"), (a)=>{
                a.querySelector(".button.delete").addEventListener("click", (e)=>a.remove());
            });
        }
        var trace = document.querySelector(".trace");
        if(trace){
            var req = new XMLHttpRequest();
            req.open("GET", trace.dataset.url, true);
            req.responseType = "arraybuffer";
            if(trace.dataset.type.toLowerCase() == "2d-points"){
                if(console.log) console.log("Fetching",trace.dataset.url);
                req.onload = function(ev){
                    if(!req.response){
                        console.log("Got no response. WTF?", ev, req);
                        return;
                    }
                    var floats = new Float32Array(req.response);
                    var l=0,r=0,b=0,t=0;
                    var margin = 10;
                    for(var i=0; i<floats.length; i+=2){
                        l = Math.min(floats[i+0], l);
                        r = Math.max(floats[i+0], r);
                        b = Math.min(floats[i+1], b);
                        t = Math.max(floats[i+1], t);
                    }
                    l -= margin; r += margin; b -= margin; t += margin;
                    
                    var points = [];
                    for(var i=0; i<floats.length; i+=2){
                        points.push({x: (floats[i+0]-l)/(r-l)*trace.clientWidth,
                                     y: (floats[i+1]-b)/(t-b)*trace.clientHeight,
                                     value: 50});
                    }
                    if(console.log) console.log("Displaying",points.length,"points");
                    var heatmap = h337.create({container: trace, radius: 10});
                    heatmap.setData({min: 0, max: points.length, data: points});
                    heatmap.repaint();
                };
                req.send();
            }
        }
    }
};

var feedback = new Feedback();
