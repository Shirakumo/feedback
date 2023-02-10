class Feedback{
    constructor(){
        var self = this;
        self.cache = {};
        self.loading = {};
        if(console.log === undefined)
            self.log = ()=>{};
        else
            self.log = function(){
                var args = Array.prototype.slice.call(arguments);
                args.unshift("[Feedback]");
                return console.log.apply(console, args);
            };

        self.log("Init");

        self.apiRoot = document.querySelector("head link[rel=api-root]").getAttribute("href");
        if(!self.apiRoot){
            self.log("Failed to retrieve API root. WTF?");
        }

        self.registerElements();

        if(window.location.hash){
            let element = document.getElementById(window.location.hash.substr(1));
            if(element){
                element.classList.add("highlight");
                if(element.closest(".entry").expand) element.closest(".entry").expand().scrollIntoView({block:"nearest"});
            }
        }

        if(window.history.replaceState)
            window.history.replaceState(null, null, window.location.href);
    }

    registerElements(element){
        element = element || document;
        var self = this;
        self.registerAll(element, ".project.edit", self.registerProjectEdit);
        self.registerAll(element, ".trace", self.registerTrace);
        self.registerAll(element, ".track", self.registerTrack);
        self.registerAll(element, ".entry.view", self.registerEntry);
        self.registerAll(element, ".confirm", self.registerConfirm);
        self.registerAll(element, "time", self.registerTime);
    }

    registerAll(element, query, regger){
        var self = this;
        var elements = element.querySelectorAll(query);
        for(var i=0; i<elements.length; ++i)
            regger.apply(self, [elements[i]]);
    }

    apiCall(endpoint, args, methodArgs){
        var self = this;
        methodArgs = methodArgs || {};
        methodArgs.format = methodArgs.format || "json";
        return new Promise((ok, fail)=>{
            var request = new XMLHttpRequest();
            var formData;

            if(!(endpoint.startsWith("http://") || endpoint.startsWith("https://"))){
                endpoint = self.apiRoot+endpoint;
            }

            if(args instanceof HTMLElement){
                formData = new FormData(args);
                formData.delete("browser");
            }else if(args instanceof FormData){
                formData = args;
            }else{
                formData = new FormData();
                for(var field in args){
                    formData.append(field, args[field]);
                }
            }

            if(methodArgs.format == "json")
                formData.append("data-format", "json");
            request.onload = ()=>{
                var data = request.responseText;
                var status = request.status;
                if(request.getResponseHeader("Content-Type").includes("application/json")){
                    data = JSON.parse(data);
                    status = data.status || status;
                }
                if(status === 200){
                    self.log("Request succeeded", data);
                    ok(data);
                }else{
                    self.log("Request failed", data);
                    fail(data);
                }
            };
            const formDataObj = [];
            formData.forEach((value, key) => (formDataObj.push([key, value])));
            self.log("Sending request to",endpoint,formDataObj);
            request.open("POST", endpoint);
            request.send(formData);
        });
    }

    registerProjectEdit(element){
        var duper = function(base){
            var template = base.querySelector("li").cloneNode(true);
            base.querySelector(".button.add").addEventListener("click", (e)=>{
                var copy = template.cloneNode(true);
                copy.querySelector(".button.delete").addEventListener("click", (e)=>copy.remove());
                base.querySelector("ul").appendChild(copy);
            });
            [].forEach.call(base.querySelectorAll(".button.delete"), (a)=>{
                a.addEventListener("click", (e)=>a.parentNode.remove());
            });
        };
        duper(element.querySelector(".attachments"));
        duper(element.querySelector(".members"));
        duper(element.querySelector(".tags"));
    }

    registerTrace(element){
        var req = new XMLHttpRequest();
        req.open("GET", element.dataset.url, true);
        req.responseType = "arraybuffer";
        if(element.dataset.type.toLowerCase() == "2d-points"){
            if(console.log) console.log("Fetching",element.dataset.url);
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
                    points.push({x: (floats[i+0]-l)/(r-l)*element.clientWidth,
                                 y: (floats[i+1]-b)/(t-b)*element.clientHeight,
                                 value: 50});
                }
                if(console.log) console.log("Displaying",points.length,"points");
                var heatmap = h337.create({container: element, radius: 10});
                heatmap.setData({min: 0, max: points.length, data: points});
                heatmap.repaint();
            };
            req.send();
        }
    }

    registerTrack(element){
        var self = this;
        let entries = element.querySelectorAll(".entry");
        let ordered = [];
        element.entries = ordered;
        for(var i=0; i<entries.length; ++i){
            self.registerEntry(entries[i], element);
            if(entries[i].classList.contains("existing"))
                ordered[parseInt(entries[i].dataset.order)] = entries[i];
        }
        element.reorder = (then, now)=>{
            let movedElement = ordered[then];
            if(then < now){
                movedElement.parentNode.insertBefore(movedElement, ordered[now]);
                for(let i=then; i<now; ++i){
                    ordered[i] = ordered[i+1];
                    ordered[i].dataset.order = i;
                }
            }else{
                movedElement.parentNode.insertBefore(movedElement, ordered[now].nextSibling);
                for(let i=now; i>then; --i){
                    ordered[i] = ordered[i-1];
                    ordered[i].dataset.order = i;
                }
            }
            ordered[now] = movedElement;
            movedElement.dataset.order = now;
            return element;
        };
    }

    registerEntry(element, track){
        var self = this;
        if(element.classList.contains("existing")){
            let collapse = element.querySelector(".collapse");
            let extra = element.querySelector(".extra");
            let drag = element.querySelector(".drag");

            element.expand = ()=>{
                collapse.querySelector("i").classList.remove("fa-chevron-up");
                collapse.querySelector("i").classList.add("fa-chevron-down");
                extra.classList.remove("collapsed");
                // Smooth transition
                extra.style.height = "0px";
                requestAnimationFrame(()=>extra.style.height = extra.scrollHeight+'px');
                return element;
            };

            element.collapse = ()=>{
                collapse.querySelector("i").classList.remove("fa-chevron-down");
                collapse.querySelector("i").classList.add("fa-chevron-up");
                extra.classList.add("collapsed");
                // Smooth transition
                let height = extra.scrollHeight;
                let transition = extra.style.transition;
                extra.style.transition = '';
                requestAnimationFrame(()=>{
                    extra.style.height = height+'px';
                    extra.style.transition = transition;
                    requestAnimationFrame(()=>extra.style.height = '');
                });
                return element;
            };

            element.querySelector("form.row").addEventListener("dblclick", (ev)=>{
                extra.querySelector(".detail").click();
            });

            self.registerAll(element, ".row input,.row select", (el)=>{
                el.addEventListener("change", (ev)=>self.submitForm(el));
            });

            collapse.addEventListener("click", (ev)=>{
                if(extra.classList.contains("collapsed"))
                    element.expand();
                else
                    element.collapse();
                ev.preventDefault();
                return false;
            });

            drag.addEventListener("mousedown", (ev)=>{
                element.setAttribute("draggable", "true");
            });
            element.addEventListener("dragstart", (ev)=>{
                ev.dataTransfer.setData("text/plain", element.dataset.id);
            });
            element.addEventListener("dragend", (ev)=>{
                element.removeAttribute("draggable");
            });
            element.addEventListener("dragenter", (ev)=>{
                ev.preventDefault();
                element.classList.add("dragover");
            });
            element.addEventListener("dragleave", (ev)=>{
                ev.preventDefault();
                element.classList.remove("dragover");
            });
            element.addEventListener("dragover", (ev)=>{
                ev.preventDefault();
                ev.dataTransfer.dropEffect = "move";
            });
            element.addEventListener("drop", (ev)=>{
                ev.preventDefault();
                element.classList.remove("dragover");
                let sourceID = ev.dataTransfer.getData("text/plain");
                if(sourceID){
                    let target = document.querySelector(".entry[data-id=\""+sourceID+"\"]");
                    self.apiCall("entry/edit", {entry: sourceID, order: element.dataset.order})
                        .then(()=>track.reorder(parseInt(target.dataset.order), parseInt(element.dataset.order)));
                }
            });
        }

        self.registerAll(element, ".note.existing", self.registerNote);
        
        self.registerAll(element, "textarea", (el)=>{
            el.fit = ()=>{
                el.style.height = '1px';
                el.style.height = Math.max(el.scrollHeight, 20) + "px";
            };
            el.fit();
            el.addEventListener("input", el.fit);
            el.addEventListener("keypress", (ev)=>{
                if(ev.which === 13 && !ev.shiftKey && !ev.ctrlKey) {
                    ev.preventDefault();
                    self.submitForm(el);
                }
            });
        });

        let parseEntry = (entry)=>self.parseIdCode(entry.substr(0, entry.indexOf(" ")));
        self.registerAll(element, "a.duplicate", (el)=>{
            el.addEventListener("click", (ev)=>{
                ev.preventDefault();
                self.queryListed("entry/list", {project: element.dataset.project},
                                 (e)=>self.formatIdCode(e._id)+" "+self.formatShort(e.description))
                    .then((entry)=>self.apiCall("entry/edit", {
                        "entry": element.dataset.id,
                        "status": "duplicate",
                        "order": "bottom",
                        "relates-to": parseEntry(entry)}))
                    .then(self.handleResponse);
            });
        });

        self.registerAll(element, "a.related", (el)=>{
            el.addEventListener("click", (ev)=>{
                ev.preventDefault();
                self.queryListed("entry/list", {project: element.dataset.project},
                                 (e)=>self.formatIdCode(e._id)+" "+self.formatShort(e.description))
                    .then((entry)=>self.apiCall("entry/edit", {
                        "entry": element.dataset.id,
                        "relates-to": parseEntry(entry)}))
                    .then(self.handleResponse);
            });
        });

        self.registerAll(element, "a.retrack", (el)=>{
            el.addEventListener("click", (ev)=>{
                ev.preventDefault();
                self.queryListed("track/list", {project: element.dataset.project},
                                 (e)=>e._id+" "+e.name)
                    .then((entry)=>self.apiCall("entry/edit", {
                        "entry": element.dataset.id,
                        "track": entry.substr(0, entry.indexOf(" "))}))
                    .then(self.handleResponse);
            });
        });

        self.registerAll(element, ".tags", self.registerTags);
    }

    registerNote(element){
        var self = this;
        element.querySelector("p.text").addEventListener("dblclick", (ev)=>{
            element.querySelector("textarea.text").style.display = "block";
            element.querySelector("p.text").style.display = "none";
        });
    }

    registerConfirm(element){
        element.addEventListener("click", (ev)=>{
            if(confirm("Are you sure?")){
                return true;
            }else{
                ev.preventDefault();
                return false;
            }
        });
    }

    registerTime(element){
        var self = this;
        element.innerText = self.formatTime(new Date(Date.parse(element.getAttribute("datetime")+"Z")));
        if(element.classList.contains("now"))
           setInterval(()=>{
               element.innerText = self.formatTime();
           }, 1000);
    }

    registerTags(element){
        var self = this;

        let entry = element.closest(".entry");
        self.registerAll(element, "span.tag", self.registerTag);

        element.addEventListener("click", ()=>{
            let constructor = element.querySelector("input");
            if(!constructor){
                constructor = document.createElement("input");
                constructor.setAttribute("list", "tags");
                constructor.addEventListener("change", (ev)=>{
                    ev.preventDefault();
                    if(constructor.value){
                        self.apiCall("entry/tag/new", {tag: constructor.value, entry: entry.dataset.id})
                            .then((response)=>{
                                let tag = document.createElement("span");
                                let color = response.data.color.toString(16);
                                while(color.length < 6) color = "0"+color;
                                tag.dataset.color = "#"+color;
                                tag.classList.add("tag");
                                tag.innerText = response.data.name;
                                element.insertBefore(tag, constructor);
                                self.registerTag(tag);
                                constructor.classList.add("hidden");
                            });
                    }
                });
                element.appendChild(constructor);
            }
            constructor.value = "";
            constructor.classList.remove("hidden");
            constructor.focus();
        });
    }

    registerTag(element){
        var self = this;
        let entry = element.closest(".entry");
        let color = element.dataset.color;
        var r = parseInt(color.substring(1,3),16);
        var g = parseInt(color.substring(3,5),16);
        var b = parseInt(color.substring(5,7),16);
        var yiq = ((r*299)+(g*587)+(b*114))/1000;
        element.style.background = color;
        element.style.color = (yiq >= 128) ? 'black' : 'white';

        element.addEventListener("click", (ev)=>ev.stopPropagation());
        element.addEventListener("dblclick", (ev)=>{
            ev.preventDefault();
            ev.stopPropagation();
            if(element.classList.contains("removed")){
                self.apiCall("entry/tag/new", {tag: element.innerText, entry: entry.dataset.id})
                    .then(()=>element.classList.remove("removed"));
            }else{
                self.apiCall("entry/tag/delete", {tag: element.innerText, entry: entry.dataset.id})
                    .then(()=>element.classList.add("removed"));
            }
        });
    }

    formatTime(stamp){
        stamp = stamp || new Date();
        let p = (f)=>(f<10?"0"+f:f);
        return p(stamp.getFullYear())+"."+p(stamp.getMonth()+1)+"."+p(stamp.getDate())+" "
            + p(stamp.getHours())+":"+p(stamp.getMinutes())+":"+p(stamp.getSeconds());
    }

    formatIdCode(id){
        let code = id.toString(16);
        while(code.length < 4) code = "0"+code;
        return "$"+code;
    }

    parseIdCode(code){
        return parseInt(code.substr(1), 16);
    }

    formatShort(text, len){
        len = len || 36;
        return (text.length <= len)? text : text.substr(0, 33)+"...";
    }

    submitForm(el){
        let form = el.closest("form");
        if(form.querySelector('input[type="submit"]'))
            form.querySelector('input[type="submit"]').click();
        else form.submit();
    }

    showMessage(msg){
        document.querySelector(".box.info").innerText = msg;
    }

    showError(msg){
        document.querySelector(".box.error").innerText = msg;
    }

    handleResponse(result){
        if(result.target && window.location.href == result.target)
            window.location.reload();
        if(result.target)
            window.location.replace(result.target);
        else if(result.message) 
            self.showMessage(result.message);
    }

    queryListed(api, data, present){
        var self = this;
        return new Promise((ok, fail)=>{
            let popup = document.querySelector(".popup");
            let input = popup.querySelector("input[name=entry]");
            let datalist = popup.querySelector("datalist");
            let close; close = ()=>{
                popup.removeEventListener("click", close);
                popup.style.display = null;
                datalist.innerHTML = "";
                input.value = "";
            };
            popup.style.display = "block";
            requestAnimationFrame(()=>popup.querySelector("input").focus());
            popup.addEventListener("click", (ev)=>{
                if(ev.target == popup){close();fail();}});

            input.addEventListener("input", (ev)=>{
                if(0 < input.value.length){
                    data.query = input.value;
                    self.apiCall(api, data)
                        .then((r)=>{
                            if(r.status == 200 && r.data){
                                datalist.innerHTML = "";
                                r.data.forEach((e)=>{
                                    let option = document.createElement("option");
                                    option.setAttribute("value", present(e));
                                    datalist.appendChild(option);
                                });
                            }
                        });
                }
            });

            popup.querySelector("form").addEventListener("submit", (ev)=>{
                ev.preventDefault();
                ok(input.value);
                close();
            });
        });
    }
    
};

var feedback;
document.addEventListener("DOMContentLoaded", ()=>feedback = feedback || new Feedback());
