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

            if(trace.dataset.type == "2d-points"){
                req.onload = function(ev){
                    var floats = new Float32Array(ev.response);
                    var points = [];
                    for(var i=0; i<floats.length; i+=2){
                        points.push({x: points[i+0], y: points[i+1]});
                    }
                    var heatmap = h337.create({container: trace});
                    heatmap.setData({min: 0, max: points.length, data: points});
                };
            }
        }
    }
};

var feedback = new Feedback();
