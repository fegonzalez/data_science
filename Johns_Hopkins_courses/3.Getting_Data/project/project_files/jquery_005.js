!function($,undefined){"use strict";function debug(s){if($.fn.cycle.debug)log(s)}function log(){if(window.console&&console.log)console.log("[cycle] "+Array.prototype.join.call(arguments," "))}function triggerPause(cont,byHover,onPager){var opts=$(cont).data("cycle.opts"),paused=!!cont.cyclePause;if(paused&&opts.paused)opts.paused(cont,opts,byHover,onPager);else if(!paused&&opts.resumed)opts.resumed(cont,opts,byHover,onPager)}function handleArguments(cont,options,arg2){function checkInstantResume(isPaused,arg2,cont){if(!isPaused&&arg2===!0){var options=$(cont).data("cycle.opts");if(!options)return log("options not found, can not resume"),!1;if(cont.cycleTimeout)clearTimeout(cont.cycleTimeout),cont.cycleTimeout=0;go(options.elements,options,1,!options.backwards)}}if(cont.cycleStop===undefined)cont.cycleStop=0;if(options===undefined||null===options)options={};if(options.constructor==String){switch(options){case"destroy":case"stop":var opts=$(cont).data("cycle.opts");if(!opts)return!1;if(cont.cycleStop++,cont.cycleTimeout)clearTimeout(cont.cycleTimeout);if(cont.cycleTimeout=0,opts.elements)$(opts.elements).stop();if($(cont).removeData("cycle.opts"),"destroy"==options)destroy(cont,opts);return!1;case"toggle":return cont.cyclePause=1===cont.cyclePause?0:1,checkInstantResume(cont.cyclePause,arg2,cont),triggerPause(cont),!1;case"pause":return cont.cyclePause=1,triggerPause(cont),!1;case"resume":return cont.cyclePause=0,checkInstantResume(!1,arg2,cont),triggerPause(cont),!1;case"prev":case"next":if(opts=$(cont).data("cycle.opts"),!opts)return log('options not found, "prev/next" ignored'),!1;return $.fn.cycle[options](opts),!1;default:options={fx:options}}return options}else if(options.constructor==Number){var num=options;if(options=$(cont).data("cycle.opts"),!options)return log("options not found, can not advance slide"),!1;if(0>num||num>=options.elements.length)return log("invalid slide index: "+num),!1;if(options.nextSlide=num,cont.cycleTimeout)clearTimeout(cont.cycleTimeout),cont.cycleTimeout=0;if("string"==typeof arg2)options.oneTimeFx=arg2;return go(options.elements,options,1,num>=options.currSlide),!1}return options}function removeFilter(el,opts){if(!$.support.opacity&&opts.cleartype&&el.style.filter)try{el.style.removeAttribute("filter")}catch(smother){}}function destroy(cont,opts){if(opts.next)$(opts.next).unbind(opts.prevNextEvent);if(opts.prev)$(opts.prev).unbind(opts.prevNextEvent);if(opts.pager||opts.pagerAnchorBuilder)$.each(opts.pagerAnchors||[],function(){this.unbind().remove()});if(opts.pagerAnchors=null,$(cont).unbind("mouseenter.cycle mouseleave.cycle"),opts.destroy)opts.destroy(opts)}function buildOptions($cont,$slides,els,options,o){var startingSlideSpecified,opts=$.extend({},$.fn.cycle.defaults,options||{},$.metadata?$cont.metadata():$.meta?$cont.data():{}),meta=$.isFunction($cont.data)?$cont.data(opts.metaAttr):null;if(meta)opts=$.extend(opts,meta);if(opts.autostop)opts.countdown=opts.autostopCount||els.length;var cont=$cont[0];if($cont.data("cycle.opts",opts),opts.$cont=$cont,opts.stopCount=cont.cycleStop,opts.elements=els,opts.before=opts.before?[opts.before]:[],opts.after=opts.after?[opts.after]:[],!$.support.opacity&&opts.cleartype)opts.after.push(function(){removeFilter(this,opts)});if(opts.continuous)opts.after.push(function(){go(els,opts,0,!opts.backwards)});if(saveOriginalOpts(opts),!$.support.opacity&&opts.cleartype&&!opts.cleartypeNoBg)clearTypeFix($slides);if("static"==$cont.css("position"))$cont.css("position","relative");if(opts.width)$cont.width(opts.width);if(opts.height&&"auto"!=opts.height)$cont.height(opts.height);if(opts.startingSlide!==undefined)if(opts.startingSlide=parseInt(opts.startingSlide,10),opts.startingSlide>=els.length||opts.startSlide<0)opts.startingSlide=0;else startingSlideSpecified=!0;else if(opts.backwards)opts.startingSlide=els.length-1;else opts.startingSlide=0;if(opts.random){opts.randomMap=[];for(var i=0;i<els.length;i++)opts.randomMap.push(i);if(opts.randomMap.sort(function(a,b){return Math.random()-.5}),startingSlideSpecified){for(var cnt=0;cnt<els.length;cnt++)if(opts.startingSlide==opts.randomMap[cnt])opts.randomIndex=cnt}else opts.randomIndex=1,opts.startingSlide=opts.randomMap[1]}else if(opts.startingSlide>=els.length)opts.startingSlide=0;opts.currSlide=opts.startingSlide||0;var first=opts.startingSlide;if($slides.css({position:"absolute",top:0,left:0}).hide().each(function(i){var z;if(opts.backwards)z=first?first>=i?els.length+(i-first):first-i:els.length-i;else z=first?i>=first?els.length-(i-first):first-i:els.length-i;$(this).css("z-index",z)}),$(els[first]).css("opacity",1).show(),removeFilter(els[first],opts),opts.fit)if(!opts.aspect){if(opts.width)$slides.width(opts.width);if(opts.height&&"auto"!=opts.height)$slides.height(opts.height)}else $slides.each(function(){var $slide=$(this),ratio=opts.aspect===!0?$slide.width()/$slide.height():opts.aspect;if(opts.width&&$slide.width()!=opts.width)$slide.width(opts.width),$slide.height(opts.width/ratio);if(opts.height&&$slide.height()<opts.height)$slide.height(opts.height),$slide.width(opts.height*ratio)});if(opts.center&&(!opts.fit||opts.aspect))$slides.each(function(){var $slide=$(this);$slide.css({"margin-left":opts.width?(opts.width-$slide.width())/2+"px":0,"margin-top":opts.height?(opts.height-$slide.height())/2+"px":0})});if(opts.center&&!opts.fit&&!opts.slideResize)$slides.each(function(){var $slide=$(this);$slide.css({"margin-left":opts.width?(opts.width-$slide.width())/2+"px":0,"margin-top":opts.height?(opts.height-$slide.height())/2+"px":0})});var reshape=opts.containerResize&&!$cont.innerHeight();if(reshape){for(var maxw=0,maxh=0,j=0;j<els.length;j++){var $e=$(els[j]),e=$e[0],w=$e.outerWidth(),h=$e.outerHeight();if(!w)w=e.offsetWidth||e.width||$e.attr("width");if(!h)h=e.offsetHeight||e.height||$e.attr("height");maxw=w>maxw?w:maxw,maxh=h>maxh?h:maxh}if(maxw>0&&maxh>0)$cont.css({width:maxw+"px",height:maxh+"px"})}var pauseFlag=!1;if(opts.pause)$cont.bind("mouseenter.cycle",function(){pauseFlag=!0,this.cyclePause++,triggerPause(cont,!0)}).bind("mouseleave.cycle",function(){if(pauseFlag)this.cyclePause--;triggerPause(cont,!0)});if(supportMultiTransitions(opts)===!1)return!1;var requeue=!1;if(options.requeueAttempts=options.requeueAttempts||0,$slides.each(function(){var $el=$(this);if(this.cycleH=opts.fit&&opts.height?opts.height:$el.height()||this.offsetHeight||this.height||$el.attr("height")||0,this.cycleW=opts.fit&&opts.width?opts.width:$el.width()||this.offsetWidth||this.width||$el.attr("width")||0,$el.is("img")){var loadingIE=$.browser.msie&&28==this.cycleW&&30==this.cycleH&&!this.complete,loadingFF=$.browser.mozilla&&34==this.cycleW&&19==this.cycleH&&!this.complete,loadingOp=$.browser.opera&&(42==this.cycleW&&19==this.cycleH||37==this.cycleW&&17==this.cycleH)&&!this.complete,loadingOther=0===this.cycleH&&0===this.cycleW&&!this.complete;if(loadingIE||loadingFF||loadingOp||loadingOther)if(o.s&&opts.requeueOnImageNotLoaded&&++options.requeueAttempts<100)return log(options.requeueAttempts," - img slide not loaded, requeuing slideshow: ",this.src,this.cycleW,this.cycleH),setTimeout(function(){$(o.s,o.c).cycle(options)},opts.requeueTimeout),requeue=!0,!1;else log("could not determine size of image: "+this.src,this.cycleW,this.cycleH)}return!0}),requeue)return!1;if(opts.cssBefore=opts.cssBefore||{},opts.cssAfter=opts.cssAfter||{},opts.cssFirst=opts.cssFirst||{},opts.animIn=opts.animIn||{},opts.animOut=opts.animOut||{},$slides.not(":eq("+first+")").css(opts.cssBefore),$($slides[first]).css(opts.cssFirst),opts.timeout){if(opts.timeout=parseInt(opts.timeout,10),opts.speed.constructor==String)opts.speed=$.fx.speeds[opts.speed]||parseInt(opts.speed,10);if(!opts.sync)opts.speed=opts.speed/2;for(var buffer="none"==opts.fx?0:"shuffle"==opts.fx?500:250;opts.timeout-opts.speed<buffer;)opts.timeout+=opts.speed}if(opts.easing)opts.easeIn=opts.easeOut=opts.easing;if(!opts.speedIn)opts.speedIn=opts.speed;if(!opts.speedOut)opts.speedOut=opts.speed;if(opts.slideCount=els.length,opts.currSlide=opts.lastSlide=first,opts.random){if(++opts.randomIndex==els.length)opts.randomIndex=0;opts.nextSlide=opts.randomMap[opts.randomIndex]}else if(opts.backwards)opts.nextSlide=0===opts.startingSlide?els.length-1:opts.startingSlide-1;else opts.nextSlide=opts.startingSlide>=els.length-1?0:opts.startingSlide+1;if(!opts.multiFx){var init=$.fn.cycle.transitions[opts.fx];if($.isFunction(init))init($cont,$slides,opts);else if("custom"!=opts.fx&&!opts.multiFx)return log("unknown transition: "+opts.fx,"; slideshow terminating"),!1}var e0=$slides[first];if(!opts.skipInitializationCallbacks){if(opts.before.length)opts.before[0].apply(e0,[e0,e0,opts,!0]);if(opts.after.length)opts.after[0].apply(e0,[e0,e0,opts,!0])}if(opts.next)$(opts.next).bind(opts.prevNextEvent,function(){return advance(opts,1)});if(opts.prev)$(opts.prev).bind(opts.prevNextEvent,function(){return advance(opts,0)});if(opts.pager||opts.pagerAnchorBuilder)buildPager(els,opts);return exposeAddSlide(opts,els),opts}function saveOriginalOpts(opts){opts.original={before:[],after:[]},opts.original.cssBefore=$.extend({},opts.cssBefore),opts.original.cssAfter=$.extend({},opts.cssAfter),opts.original.animIn=$.extend({},opts.animIn),opts.original.animOut=$.extend({},opts.animOut),$.each(opts.before,function(){opts.original.before.push(this)}),$.each(opts.after,function(){opts.original.after.push(this)})}function supportMultiTransitions(opts){var i,tx,txs=$.fn.cycle.transitions;if(opts.fx.indexOf(",")>0){for(opts.multiFx=!0,opts.fxs=opts.fx.replace(/\s*/g,"").split(","),i=0;i<opts.fxs.length;i++){var fx=opts.fxs[i];if(tx=txs[fx],!tx||!txs.hasOwnProperty(fx)||!$.isFunction(tx))log("discarding unknown transition: ",fx),opts.fxs.splice(i,1),i--}if(!opts.fxs.length)return log("No valid transitions named; slideshow terminating."),!1}else if("all"==opts.fx){opts.multiFx=!0,opts.fxs=[];for(var p in txs)if(txs.hasOwnProperty(p))if(tx=txs[p],txs.hasOwnProperty(p)&&$.isFunction(tx))opts.fxs.push(p)}if(opts.multiFx&&opts.randomizeEffects){var r1=Math.floor(20*Math.random())+30;for(i=0;r1>i;i++){var r2=Math.floor(Math.random()*opts.fxs.length);opts.fxs.push(opts.fxs.splice(r2,1)[0])}debug("randomized fx sequence: ",opts.fxs)}return!0}function exposeAddSlide(opts,els){opts.addSlide=function(newSlide,prepend){var $s=$(newSlide),s=$s[0];if(!opts.autostopCount)opts.countdown++;if(els[prepend?"unshift":"push"](s),opts.els)opts.els[prepend?"unshift":"push"](s);if(opts.slideCount=els.length,opts.random)opts.randomMap.push(opts.slideCount-1),opts.randomMap.sort(function(a,b){return Math.random()-.5});if($s.css("position","absolute"),$s[prepend?"prependTo":"appendTo"](opts.$cont),prepend)opts.currSlide++,opts.nextSlide++;if(!$.support.opacity&&opts.cleartype&&!opts.cleartypeNoBg)clearTypeFix($s);if(opts.fit&&opts.width)$s.width(opts.width);if(opts.fit&&opts.height&&"auto"!=opts.height)$s.height(opts.height);if(s.cycleH=opts.fit&&opts.height?opts.height:$s.height(),s.cycleW=opts.fit&&opts.width?opts.width:$s.width(),$s.css(opts.cssBefore),opts.pager||opts.pagerAnchorBuilder)$.fn.cycle.createPagerAnchor(els.length-1,s,$(opts.pager),els,opts);if($.isFunction(opts.onAddSlide))opts.onAddSlide($s);else $s.hide()}}function go(els,opts,manual,fwd){function queueNext(){var ms=0,timeout=opts.timeout;if(opts.timeout&&!opts.continuous){if(ms=getTimeout(els[opts.currSlide],els[opts.nextSlide],opts,fwd),"shuffle"==opts.fx)ms-=opts.speedOut}else if(opts.continuous&&p.cyclePause)ms=10;if(ms>0)p.cycleTimeout=setTimeout(function(){go(els,opts,0,!opts.backwards)},ms)}var p=opts.$cont[0],curr=els[opts.currSlide],next=els[opts.nextSlide];if(manual&&opts.busy&&opts.manualTrump)debug("manualTrump in go(), stopping active transition"),$(els).stop(!0,!0),opts.busy=0,clearTimeout(p.cycleTimeout);if(opts.busy)return void debug("transition active, ignoring new tx request");if(p.cycleStop!=opts.stopCount||0===p.cycleTimeout&&!manual)return;if(!manual&&!p.cyclePause&&!opts.bounce&&(opts.autostop&&--opts.countdown<=0||opts.nowrap&&!opts.random&&opts.nextSlide<opts.currSlide)){if(opts.end)opts.end(opts);return}var changed=!1;if((manual||!p.cyclePause)&&opts.nextSlide!=opts.currSlide){changed=!0;var fx=opts.fx;if(curr.cycleH=curr.cycleH||$(curr).height(),curr.cycleW=curr.cycleW||$(curr).width(),next.cycleH=next.cycleH||$(next).height(),next.cycleW=next.cycleW||$(next).width(),opts.multiFx){if(fwd&&(opts.lastFx===undefined||++opts.lastFx>=opts.fxs.length))opts.lastFx=0;else if(!fwd&&(opts.lastFx===undefined||--opts.lastFx<0))opts.lastFx=opts.fxs.length-1;fx=opts.fxs[opts.lastFx]}if(opts.oneTimeFx)fx=opts.oneTimeFx,opts.oneTimeFx=null;if($.fn.cycle.resetState(opts,fx),opts.before.length)$.each(opts.before,function(i,o){if(p.cycleStop!=opts.stopCount)return;o.apply(next,[curr,next,opts,fwd])});var after=function(){if(opts.busy=0,$.each(opts.after,function(i,o){if(p.cycleStop!=opts.stopCount)return;o.apply(next,[curr,next,opts,fwd])}),!p.cycleStop)queueNext()};if(debug("tx firing("+fx+"); currSlide: "+opts.currSlide+"; nextSlide: "+opts.nextSlide),opts.busy=1,opts.fxFn)opts.fxFn(curr,next,opts,after,fwd,manual&&opts.fastOnEvent);else if($.isFunction($.fn.cycle[opts.fx]))$.fn.cycle[opts.fx](curr,next,opts,after,fwd,manual&&opts.fastOnEvent);else $.fn.cycle.custom(curr,next,opts,after,fwd,manual&&opts.fastOnEvent)}else queueNext();if(changed||opts.nextSlide==opts.currSlide){var roll;if(opts.lastSlide=opts.currSlide,opts.random){if(opts.currSlide=opts.nextSlide,++opts.randomIndex==els.length)opts.randomIndex=0,opts.randomMap.sort(function(a,b){return Math.random()-.5});if(opts.nextSlide=opts.randomMap[opts.randomIndex],opts.nextSlide==opts.currSlide)opts.nextSlide=opts.currSlide==opts.slideCount-1?0:opts.currSlide+1}else if(opts.backwards)if(roll=opts.nextSlide-1<0,roll&&opts.bounce)opts.backwards=!opts.backwards,opts.nextSlide=1,opts.currSlide=0;else opts.nextSlide=roll?els.length-1:opts.nextSlide-1,opts.currSlide=roll?0:opts.nextSlide+1;else if(roll=opts.nextSlide+1==els.length,roll&&opts.bounce)opts.backwards=!opts.backwards,opts.nextSlide=els.length-2,opts.currSlide=els.length-1;else opts.nextSlide=roll?0:opts.nextSlide+1,opts.currSlide=roll?els.length-1:opts.nextSlide-1}if(changed&&opts.pager)opts.updateActivePagerLink(opts.pager,opts.currSlide,opts.activePagerClass)}function getTimeout(curr,next,opts,fwd){if(opts.timeoutFn){for(var t=opts.timeoutFn.call(curr,curr,next,opts,fwd);"none"!=opts.fx&&t-opts.speed<250;)t+=opts.speed;if(debug("calculated timeout: "+t+"; speed: "+opts.speed),t!==!1)return t}return opts.timeout}function advance(opts,moveForward){var val=moveForward?1:-1,els=opts.elements,p=opts.$cont[0],timeout=p.cycleTimeout;if(timeout)clearTimeout(timeout),p.cycleTimeout=0;if(opts.random&&0>val){if(opts.randomIndex--,-2==--opts.randomIndex)opts.randomIndex=els.length-2;else if(-1==opts.randomIndex)opts.randomIndex=els.length-1;opts.nextSlide=opts.randomMap[opts.randomIndex]}else if(opts.random)opts.nextSlide=opts.randomMap[opts.randomIndex];else if(opts.nextSlide=opts.currSlide+val,opts.nextSlide<0){if(opts.nowrap)return!1;opts.nextSlide=els.length-1}else if(opts.nextSlide>=els.length){if(opts.nowrap)return!1;opts.nextSlide=0}var cb=opts.onPrevNextEvent||opts.prevNextClick;if($.isFunction(cb))cb(val>0,opts.nextSlide,els[opts.nextSlide]);return go(els,opts,1,moveForward),!1}function buildPager(els,opts){var $p=$(opts.pager);$.each(els,function(i,o){$.fn.cycle.createPagerAnchor(i,o,$p,els,opts)}),opts.updateActivePagerLink(opts.pager,opts.startingSlide,opts.activePagerClass)}function clearTypeFix($slides){function hex(s){return s=parseInt(s,10).toString(16),s.length<2?"0"+s:s}function getBg(e){for(;e&&"html"!=e.nodeName.toLowerCase();e=e.parentNode){var v=$.css(e,"background-color");if(v&&v.indexOf("rgb")>=0){var rgb=v.match(/\d+/g);return"#"+hex(rgb[0])+hex(rgb[1])+hex(rgb[2])}if(v&&"transparent"!=v)return v}return"#ffffff"}debug("applying clearType background-color hack"),$slides.each(function(){$(this).css("background-color",getBg(this))})}var ver="2.9999.5";if($.support===undefined)$.support={opacity:!$.browser.msie};$.expr[":"].paused=function(el){return el.cyclePause},$.fn.cycle=function(options,arg2){var o={s:this.selector,c:this.context};if(0===this.length&&"stop"!=options){if(!$.isReady&&o.s)return log("DOM not ready, queuing slideshow"),$(function(){$(o.s,o.c).cycle(options,arg2)}),this;return log("terminating; zero elements found by selector"+($.isReady?"":" (DOM not ready)")),this}return this.each(function(){var opts=handleArguments(this,options,arg2);if(opts===!1)return;if(opts.updateActivePagerLink=opts.updateActivePagerLink||$.fn.cycle.updateActivePagerLink,this.cycleTimeout)clearTimeout(this.cycleTimeout);this.cycleTimeout=this.cyclePause=0,this.cycleStop=0;var $cont=$(this),$slides=opts.slideExpr?$(opts.slideExpr,this):$cont.children(),els=$slides.get();if(els.length<2)return void log("terminating; too few slides: "+els.length);var opts2=buildOptions($cont,$slides,els,opts,o);if(opts2===!1)return;var startTime=opts2.continuous?10:getTimeout(els[opts2.currSlide],els[opts2.nextSlide],opts2,!opts2.backwards);if(startTime){if(startTime+=opts2.delay||0,10>startTime)startTime=10;debug("first timeout: "+startTime),this.cycleTimeout=setTimeout(function(){go(els,opts2,0,!opts.backwards)},startTime)}})},$.fn.cycle.resetState=function(opts,fx){fx=fx||opts.fx,opts.before=[],opts.after=[],opts.cssBefore=$.extend({},opts.original.cssBefore),opts.cssAfter=$.extend({},opts.original.cssAfter),opts.animIn=$.extend({},opts.original.animIn),opts.animOut=$.extend({},opts.original.animOut),opts.fxFn=null,$.each(opts.original.before,function(){opts.before.push(this)}),$.each(opts.original.after,function(){opts.after.push(this)});var init=$.fn.cycle.transitions[fx];if($.isFunction(init))init(opts.$cont,$(opts.elements),opts)},$.fn.cycle.updateActivePagerLink=function(pager,currSlide,clsName){$(pager).each(function(){$(this).children().removeClass(clsName).eq(currSlide).addClass(clsName)})},$.fn.cycle.next=function(opts){advance(opts,1)},$.fn.cycle.prev=function(opts){advance(opts,0)},$.fn.cycle.createPagerAnchor=function(i,el,$p,els,opts){var a;if($.isFunction(opts.pagerAnchorBuilder))a=opts.pagerAnchorBuilder(i,el),debug("pagerAnchorBuilder("+i+", el) returned: "+a);else a='<a href="#">'+(i+1)+"</a>";if(!a)return;var $a=$(a);if(0===$a.parents("body").length){var arr=[];if($p.length>1)$p.each(function(){var $clone=$a.clone(!0);$(this).append($clone),arr.push($clone[0])}),$a=$(arr);else $a.appendTo($p)}opts.pagerAnchors=opts.pagerAnchors||[],opts.pagerAnchors.push($a);var pagerFn=function(e){e.preventDefault(),opts.nextSlide=i;var p=opts.$cont[0],timeout=p.cycleTimeout;if(timeout)clearTimeout(timeout),p.cycleTimeout=0;var cb=opts.onPagerEvent||opts.pagerClick;if($.isFunction(cb))cb(opts.nextSlide,els[opts.nextSlide]);go(els,opts,1,opts.currSlide<i)};if(/mouseenter|mouseover/i.test(opts.pagerEvent))$a.hover(pagerFn,function(){});else $a.bind(opts.pagerEvent,pagerFn);if(!/^click/.test(opts.pagerEvent)&&!opts.allowPagerClickBubble)$a.bind("click.cycle",function(){return!1});var cont=opts.$cont[0],pauseFlag=!1;if(opts.pauseOnPagerHover)$a.hover(function(){pauseFlag=!0,cont.cyclePause++,triggerPause(cont,!0,!0)},function(){if(pauseFlag)cont.cyclePause--;triggerPause(cont,!0,!0)})},$.fn.cycle.hopsFromLast=function(opts,fwd){var hops,l=opts.lastSlide,c=opts.currSlide;if(fwd)hops=c>l?c-l:opts.slideCount-l;else hops=l>c?l-c:l+opts.slideCount-c;return hops},$.fn.cycle.commonReset=function(curr,next,opts,w,h,rev){if($(opts.elements).not(curr).hide(),"undefined"==typeof opts.cssBefore.opacity)opts.cssBefore.opacity=1;if(opts.cssBefore.display="block",opts.slideResize&&w!==!1&&next.cycleW>0)opts.cssBefore.width=next.cycleW;if(opts.slideResize&&h!==!1&&next.cycleH>0)opts.cssBefore.height=next.cycleH;opts.cssAfter=opts.cssAfter||{},opts.cssAfter.display="none",$(curr).css("zIndex",opts.slideCount+(rev===!0?1:0)),$(next).css("zIndex",opts.slideCount+(rev===!0?0:1))},$.fn.cycle.custom=function(curr,next,opts,cb,fwd,speedOverride){var $l=$(curr),$n=$(next),speedIn=opts.speedIn,speedOut=opts.speedOut,easeIn=opts.easeIn,easeOut=opts.easeOut;if($n.css(opts.cssBefore),speedOverride){if("number"==typeof speedOverride)speedIn=speedOut=speedOverride;else speedIn=speedOut=1;easeIn=easeOut=null}var fn=function(){$n.animate(opts.animIn,speedIn,easeIn,function(){cb()})};if($l.animate(opts.animOut,speedOut,easeOut,function(){if($l.css(opts.cssAfter),!opts.sync)fn()}),opts.sync)fn()},$.fn.cycle.transitions={fade:function($cont,$slides,opts){$slides.not(":eq("+opts.currSlide+")").css("opacity",0),opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts),opts.cssBefore.opacity=0}),opts.animIn={opacity:1},opts.animOut={opacity:0},opts.cssBefore={top:0,left:0}}},$.fn.cycle.ver=function(){return ver},$.fn.cycle.defaults={activePagerClass:"activeSlide",after:null,allowPagerClickBubble:!1,animIn:null,animOut:null,aspect:!1,autostop:0,autostopCount:0,backwards:!1,before:null,center:null,cleartype:!$.support.opacity,cleartypeNoBg:!1,containerResize:1,continuous:0,cssAfter:null,cssBefore:null,delay:0,easeIn:null,easeOut:null,easing:null,end:null,fastOnEvent:0,fit:0,fx:"fade",fxFn:null,height:"auto",manualTrump:!0,metaAttr:"cycle",next:null,nowrap:0,onPagerEvent:null,onPrevNextEvent:null,pager:null,pagerAnchorBuilder:null,pagerEvent:"click.cycle",pause:0,pauseOnPagerHover:0,prev:null,prevNextEvent:"click.cycle",random:0,randomizeEffects:1,requeueOnImageNotLoaded:!0,requeueTimeout:250,rev:0,shuffle:null,skipInitializationCallbacks:!1,slideExpr:null,slideResize:1,speed:1e3,speedIn:null,speedOut:null,startingSlide:undefined,sync:1,timeout:4e3,timeoutFn:null,updateActivePagerLink:null,width:null}}(jQuery),function($){"use strict";$.fn.cycle.transitions.none=function($cont,$slides,opts){opts.fxFn=function(curr,next,opts,after){$(next).show(),$(curr).hide(),after()}},$.fn.cycle.transitions.fadeout=function($cont,$slides,opts){$slides.not(":eq("+opts.currSlide+")").css({display:"block",opacity:1}),opts.before.push(function(curr,next,opts,w,h,rev){$(curr).css("zIndex",opts.slideCount+(rev!==!0?1:0)),$(next).css("zIndex",opts.slideCount+(rev!==!0?0:1))}),opts.animIn.opacity=1,opts.animOut.opacity=0,opts.cssBefore.opacity=1,opts.cssBefore.display="block",opts.cssAfter.zIndex=0},$.fn.cycle.transitions.scrollUp=function($cont,$slides,opts){$cont.css("overflow","hidden"),opts.before.push($.fn.cycle.commonReset);var h=$cont.height();opts.cssBefore.top=h,opts.cssBefore.left=0,opts.cssFirst.top=0,opts.animIn.top=0,opts.animOut.top=-h},$.fn.cycle.transitions.scrollDown=function($cont,$slides,opts){$cont.css("overflow","hidden"),opts.before.push($.fn.cycle.commonReset);var h=$cont.height();opts.cssFirst.top=0,opts.cssBefore.top=-h,opts.cssBefore.left=0,opts.animIn.top=0,opts.animOut.top=h},$.fn.cycle.transitions.scrollLeft=function($cont,$slides,opts){$cont.css("overflow","hidden"),opts.before.push($.fn.cycle.commonReset);var w=$cont.width();opts.cssFirst.left=0,opts.cssBefore.left=w,opts.cssBefore.top=0,opts.animIn.left=0,opts.animOut.left=0-w},$.fn.cycle.transitions.scrollRight=function($cont,$slides,opts){$cont.css("overflow","hidden"),opts.before.push($.fn.cycle.commonReset);var w=$cont.width();opts.cssFirst.left=0,opts.cssBefore.left=-w,opts.cssBefore.top=0,opts.animIn.left=0,opts.animOut.left=w},$.fn.cycle.transitions.scrollHorz=function($cont,$slides,opts){$cont.css("overflow","hidden").width(),opts.before.push(function(curr,next,opts,fwd){if(opts.rev)fwd=!fwd;$.fn.cycle.commonReset(curr,next,opts),opts.cssBefore.left=fwd?next.cycleW-1:1-next.cycleW,opts.animOut.left=fwd?-curr.cycleW:curr.cycleW}),opts.cssFirst.left=0,opts.cssBefore.top=0,opts.animIn.left=0,opts.animOut.top=0},$.fn.cycle.transitions.scrollVert=function($cont,$slides,opts){$cont.css("overflow","hidden"),opts.before.push(function(curr,next,opts,fwd){if(opts.rev)fwd=!fwd;$.fn.cycle.commonReset(curr,next,opts),opts.cssBefore.top=fwd?1-next.cycleH:next.cycleH-1,opts.animOut.top=fwd?curr.cycleH:-curr.cycleH}),opts.cssFirst.top=0,opts.cssBefore.left=0,opts.animIn.top=0,opts.animOut.left=0},$.fn.cycle.transitions.slideX=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$(opts.elements).not(curr).hide(),$.fn.cycle.commonReset(curr,next,opts,!1,!0),opts.animIn.width=next.cycleW}),opts.cssBefore.left=0,opts.cssBefore.top=0,opts.cssBefore.width=0,opts.animIn.width="show",opts.animOut.width=0},$.fn.cycle.transitions.slideY=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$(opts.elements).not(curr).hide(),$.fn.cycle.commonReset(curr,next,opts,!0,!1),opts.animIn.height=next.cycleH}),opts.cssBefore.left=0,opts.cssBefore.top=0,opts.cssBefore.height=0,opts.animIn.height="show",opts.animOut.height=0},$.fn.cycle.transitions.shuffle=function($cont,$slides,opts){var i,w=$cont.css("overflow","visible").width();if($slides.css({left:0,top:0}),opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!0,!0,!0)}),!opts.speedAdjusted)opts.speed=opts.speed/2,opts.speedAdjusted=!0;for(opts.random=0,opts.shuffle=opts.shuffle||{left:-w,top:15},opts.els=[],i=0;i<$slides.length;i++)opts.els.push($slides[i]);for(i=0;i<opts.currSlide;i++)opts.els.push(opts.els.shift());opts.fxFn=function(curr,next,opts,cb,fwd){if(opts.rev)fwd=!fwd;var $el=fwd?$(curr):$(next);$(next).css(opts.cssBefore);var count=opts.slideCount;$el.animate(opts.shuffle,opts.speedIn,opts.easeIn,function(){for(var hops=$.fn.cycle.hopsFromLast(opts,fwd),k=0;hops>k;k++)if(fwd)opts.els.push(opts.els.shift());else opts.els.unshift(opts.els.pop());if(fwd)for(var i=0,len=opts.els.length;len>i;i++)$(opts.els[i]).css("z-index",len-i+count);else{var z=$(curr).css("z-index");$el.css("z-index",parseInt(z,10)+1+count)}$el.animate({left:0,top:0},opts.speedOut,opts.easeOut,function(){if($(fwd?this:curr).hide(),cb)cb()})})},$.extend(opts.cssBefore,{display:"block",opacity:1,top:0,left:0})},$.fn.cycle.transitions.turnUp=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!0,!1),opts.cssBefore.top=next.cycleH,opts.animIn.height=next.cycleH,opts.animOut.width=next.cycleW}),opts.cssFirst.top=0,opts.cssBefore.left=0,opts.cssBefore.height=0,opts.animIn.top=0,opts.animOut.height=0},$.fn.cycle.transitions.turnDown=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!0,!1),opts.animIn.height=next.cycleH,opts.animOut.top=curr.cycleH}),opts.cssFirst.top=0,opts.cssBefore.left=0,opts.cssBefore.top=0,opts.cssBefore.height=0,opts.animOut.height=0},$.fn.cycle.transitions.turnLeft=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!1,!0),opts.cssBefore.left=next.cycleW,opts.animIn.width=next.cycleW}),opts.cssBefore.top=0,opts.cssBefore.width=0,opts.animIn.left=0,opts.animOut.width=0},$.fn.cycle.transitions.turnRight=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!1,!0),opts.animIn.width=next.cycleW,opts.animOut.left=curr.cycleW}),$.extend(opts.cssBefore,{top:0,left:0,width:0}),opts.animIn.left=0,opts.animOut.width=0},$.fn.cycle.transitions.zoom=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!1,!1,!0),opts.cssBefore.top=next.cycleH/2,opts.cssBefore.left=next.cycleW/2,$.extend(opts.animIn,{top:0,left:0,width:next.cycleW,height:next.cycleH}),$.extend(opts.animOut,{width:0,height:0,top:curr.cycleH/2,left:curr.cycleW/2})}),opts.cssFirst.top=0,opts.cssFirst.left=0,opts.cssBefore.width=0,opts.cssBefore.height=0},$.fn.cycle.transitions.fadeZoom=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!1,!1),opts.cssBefore.left=next.cycleW/2,opts.cssBefore.top=next.cycleH/2,$.extend(opts.animIn,{top:0,left:0,width:next.cycleW,height:next.cycleH})}),opts.cssBefore.width=0,opts.cssBefore.height=0,opts.animOut.opacity=0},$.fn.cycle.transitions.blindX=function($cont,$slides,opts){var w=$cont.css("overflow","hidden").width();opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts),opts.animIn.width=next.cycleW,opts.animOut.left=curr.cycleW}),opts.cssBefore.left=w,opts.cssBefore.top=0,opts.animIn.left=0,opts.animOut.left=w},$.fn.cycle.transitions.blindY=function($cont,$slides,opts){var h=$cont.css("overflow","hidden").height();opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts),opts.animIn.height=next.cycleH,opts.animOut.top=curr.cycleH}),opts.cssBefore.top=h,opts.cssBefore.left=0,opts.animIn.top=0,opts.animOut.top=h},$.fn.cycle.transitions.blindZ=function($cont,$slides,opts){var h=$cont.css("overflow","hidden").height(),w=$cont.width();opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts),opts.animIn.height=next.cycleH,opts.animOut.top=curr.cycleH}),opts.cssBefore.top=h,opts.cssBefore.left=w,opts.animIn.top=0,opts.animIn.left=0,opts.animOut.top=h,opts.animOut.left=w},$.fn.cycle.transitions.growX=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!1,!0),opts.cssBefore.left=this.cycleW/2,opts.animIn.left=0,opts.animIn.width=this.cycleW,opts.animOut.left=0}),opts.cssBefore.top=0,opts.cssBefore.width=0},$.fn.cycle.transitions.growY=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!0,!1),opts.cssBefore.top=this.cycleH/2,opts.animIn.top=0,opts.animIn.height=this.cycleH,opts.animOut.top=0}),opts.cssBefore.height=0,opts.cssBefore.left=0},$.fn.cycle.transitions.curtainX=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!1,!0,!0),opts.cssBefore.left=next.cycleW/2,opts.animIn.left=0,opts.animIn.width=this.cycleW,opts.animOut.left=curr.cycleW/2,opts.animOut.width=0}),opts.cssBefore.top=0,opts.cssBefore.width=0},$.fn.cycle.transitions.curtainY=function($cont,$slides,opts){opts.before.push(function(curr,next,opts){$.fn.cycle.commonReset(curr,next,opts,!0,!1,!0),opts.cssBefore.top=next.cycleH/2,opts.animIn.top=0,opts.animIn.height=next.cycleH,opts.animOut.top=curr.cycleH/2,opts.animOut.height=0}),opts.cssBefore.height=0,opts.cssBefore.left=0},$.fn.cycle.transitions.cover=function($cont,$slides,opts){var d=opts.direction||"left",w=$cont.css("overflow","hidden").width(),h=$cont.height();opts.before.push(function(curr,next,opts){if($.fn.cycle.commonReset(curr,next,opts),"right"==d)opts.cssBefore.left=-w;else if("up"==d)opts.cssBefore.top=h;else if("down"==d)opts.cssBefore.top=-h;else opts.cssBefore.left=w}),opts.animIn.left=0,opts.animIn.top=0,opts.cssBefore.top=0,opts.cssBefore.left=0},$.fn.cycle.transitions.uncover=function($cont,$slides,opts){var d=opts.direction||"left",w=$cont.css("overflow","hidden").width(),h=$cont.height();opts.before.push(function(curr,next,opts){if($.fn.cycle.commonReset(curr,next,opts,!0,!0,!0),"right"==d)opts.animOut.left=w;else if("up"==d)opts.animOut.top=-h;else if("down"==d)opts.animOut.top=h;else opts.animOut.left=-w}),opts.animIn.left=0,opts.animIn.top=0,opts.cssBefore.top=0,opts.cssBefore.left=0},$.fn.cycle.transitions.toss=function($cont,$slides,opts){var w=$cont.css("overflow","visible").width(),h=$cont.height();opts.before.push(function(curr,next,opts){if($.fn.cycle.commonReset(curr,next,opts,!0,!0,!0),!opts.animOut.left&&!opts.animOut.top)$.extend(opts.animOut,{left:2*w,top:-h/2,opacity:0});else opts.animOut.opacity=0}),opts.cssBefore.left=0,opts.cssBefore.top=0,opts.animIn.left=0},$.fn.cycle.transitions.wipe=function($cont,$slides,opts){var w=$cont.css("overflow","hidden").width(),h=$cont.height();opts.cssBefore=opts.cssBefore||{};var clip;if(opts.clip)if(/l2r/.test(opts.clip))clip="rect(0px 0px "+h+"px 0px)";else if(/r2l/.test(opts.clip))clip="rect(0px "+w+"px "+h+"px "+w+"px)";else if(/t2b/.test(opts.clip))clip="rect(0px "+w+"px 0px 0px)";
else if(/b2t/.test(opts.clip))clip="rect("+h+"px "+w+"px "+h+"px 0px)";else if(/zoom/.test(opts.clip)){var top=parseInt(h/2,10),left=parseInt(w/2,10);clip="rect("+top+"px "+left+"px "+top+"px "+left+"px)"}opts.cssBefore.clip=opts.cssBefore.clip||clip||"rect(0px 0px 0px 0px)";var d=opts.cssBefore.clip.match(/(\d+)/g),t=parseInt(d[0],10),r=parseInt(d[1],10),b=parseInt(d[2],10),l=parseInt(d[3],10);opts.before.push(function(curr,next,opts){if(curr==next)return;var $curr=$(curr),$next=$(next);$.fn.cycle.commonReset(curr,next,opts,!0,!0,!1),opts.cssAfter.display="block";var step=1,count=parseInt(opts.speedIn/13,10)-1;!function f(){var tt=t?t-parseInt(step*(t/count),10):0,ll=l?l-parseInt(step*(l/count),10):0,bb=h>b?b+parseInt(step*((h-b)/count||1),10):h,rr=w>r?r+parseInt(step*((w-r)/count||1),10):w;$next.css({clip:"rect("+tt+"px "+rr+"px "+bb+"px "+ll+"px)"}),step++<=count?setTimeout(f,13):$curr.css("display","none")}()}),$.extend(opts.cssBefore,{display:"block",opacity:1,top:0,left:0}),opts.animIn={left:0},opts.animOut={left:0}}}(jQuery);