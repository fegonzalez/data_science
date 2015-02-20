(function(){function eq(a,b,stack){if(a===b)return 0!==a||1/a==1/b;if(null==a||null==b)return a===b;if(a._chain)a=a._wrapped;if(b._chain)b=b._wrapped;if(a.isEqual&&_.isFunction(a.isEqual))return a.isEqual(b);if(b.isEqual&&_.isFunction(b.isEqual))return b.isEqual(a);var className=toString.call(a);if(className!=toString.call(b))return!1;switch(className){case"[object String]":return a==String(b);case"[object Number]":return a!=+a?b!=+b:0==a?1/a==1/b:a==+b;case"[object Date]":case"[object Boolean]":return+a==+b;case"[object RegExp]":return a.source==b.source&&a.global==b.global&&a.multiline==b.multiline&&a.ignoreCase==b.ignoreCase}if("object"!=typeof a||"object"!=typeof b)return!1;for(var length=stack.length;length--;)if(stack[length]==a)return!0;stack.push(a);var size=0,result=!0;if("[object Array]"==className){if(size=a.length,result=size==b.length)for(;size--&&(result=size in a==size in b&&eq(a[size],b[size],stack)););}else{if("constructor"in a!="constructor"in b||a.constructor!=b.constructor)return!1;for(var key in a)if(_.has(a,key))if(size++,!(result=_.has(b,key)&&eq(a[key],b[key],stack)))break;if(result){for(key in b)if(_.has(b,key)&&!size--)break;result=!size}}return stack.pop(),result}var root=this,previousUnderscore=root._,breaker={},ArrayProto=Array.prototype,ObjProto=Object.prototype,FuncProto=Function.prototype,slice=ArrayProto.slice,unshift=ArrayProto.unshift,toString=ObjProto.toString,hasOwnProperty=ObjProto.hasOwnProperty,nativeForEach=ArrayProto.forEach,nativeMap=ArrayProto.map,nativeReduce=ArrayProto.reduce,nativeReduceRight=ArrayProto.reduceRight,nativeFilter=ArrayProto.filter,nativeEvery=ArrayProto.every,nativeSome=ArrayProto.some,nativeIndexOf=ArrayProto.indexOf,nativeLastIndexOf=ArrayProto.lastIndexOf,nativeIsArray=Array.isArray,nativeKeys=Object.keys,nativeBind=FuncProto.bind,_=function(obj){return new wrapper(obj)};if("undefined"!=typeof exports){if("undefined"!=typeof module&&module.exports)exports=module.exports=_;exports._=_}else root._=_;_.VERSION="1.3.1";var each=_.each=_.forEach=function(obj,iterator,context){if(null==obj)return;if(nativeForEach&&obj.forEach===nativeForEach)obj.forEach(iterator,context);else if(obj.length===+obj.length){for(var i=0,l=obj.length;l>i;i++)if(i in obj&&iterator.call(context,obj[i],i,obj)===breaker)return}else for(var key in obj)if(_.has(obj,key))if(iterator.call(context,obj[key],key,obj)===breaker)return};_.map=_.collect=function(obj,iterator,context){var results=[];if(null==obj)return results;if(nativeMap&&obj.map===nativeMap)return obj.map(iterator,context);if(each(obj,function(value,index,list){results[results.length]=iterator.call(context,value,index,list)}),obj.length===+obj.length)results.length=obj.length;return results},_.reduce=_.foldl=_.inject=function(obj,iterator,memo,context){var initial=arguments.length>2;if(null==obj)obj=[];if(nativeReduce&&obj.reduce===nativeReduce){if(context)iterator=_.bind(iterator,context);return initial?obj.reduce(iterator,memo):obj.reduce(iterator)}if(each(obj,function(value,index,list){if(!initial)memo=value,initial=!0;else memo=iterator.call(context,memo,value,index,list)}),!initial)throw new TypeError("Reduce of empty array with no initial value");return memo},_.reduceRight=_.foldr=function(obj,iterator,memo,context){var initial=arguments.length>2;if(null==obj)obj=[];if(nativeReduceRight&&obj.reduceRight===nativeReduceRight){if(context)iterator=_.bind(iterator,context);return initial?obj.reduceRight(iterator,memo):obj.reduceRight(iterator)}var reversed=_.toArray(obj).reverse();if(context&&!initial)iterator=_.bind(iterator,context);return initial?_.reduce(reversed,iterator,memo,context):_.reduce(reversed,iterator)},_.find=_.detect=function(obj,iterator,context){var result;return any(obj,function(value,index,list){if(iterator.call(context,value,index,list))return result=value,!0}),result},_.filter=_.select=function(obj,iterator,context){var results=[];if(null==obj)return results;if(nativeFilter&&obj.filter===nativeFilter)return obj.filter(iterator,context);return each(obj,function(value,index,list){if(iterator.call(context,value,index,list))results[results.length]=value}),results},_.reject=function(obj,iterator,context){var results=[];if(null==obj)return results;return each(obj,function(value,index,list){if(!iterator.call(context,value,index,list))results[results.length]=value}),results},_.every=_.all=function(obj,iterator,context){var result=!0;if(null==obj)return result;if(nativeEvery&&obj.every===nativeEvery)return obj.every(iterator,context);return each(obj,function(value,index,list){if(!(result=result&&iterator.call(context,value,index,list)))return breaker}),result};var any=_.some=_.any=function(obj,iterator,context){iterator||(iterator=_.identity);var result=!1;if(null==obj)return result;if(nativeSome&&obj.some===nativeSome)return obj.some(iterator,context);return each(obj,function(value,index,list){if(result||(result=iterator.call(context,value,index,list)))return breaker}),!!result};_.include=_.contains=function(obj,target){var found=!1;if(null==obj)return found;if(nativeIndexOf&&obj.indexOf===nativeIndexOf)return-1!=obj.indexOf(target);return found=any(obj,function(value){return value===target})},_.invoke=function(obj,method){var args=slice.call(arguments,2);return _.map(obj,function(value){return(_.isFunction(method)?method||value:value[method]).apply(value,args)})},_.pluck=function(obj,key){return _.map(obj,function(value){return value[key]})},_.max=function(obj,iterator,context){if(!iterator&&_.isArray(obj))return Math.max.apply(Math,obj);if(!iterator&&_.isEmpty(obj))return-1/0;var result={computed:-1/0};return each(obj,function(value,index,list){var computed=iterator?iterator.call(context,value,index,list):value;computed>=result.computed&&(result={value:value,computed:computed})}),result.value},_.min=function(obj,iterator,context){if(!iterator&&_.isArray(obj))return Math.min.apply(Math,obj);if(!iterator&&_.isEmpty(obj))return 1/0;var result={computed:1/0};return each(obj,function(value,index,list){var computed=iterator?iterator.call(context,value,index,list):value;computed<result.computed&&(result={value:value,computed:computed})}),result.value},_.shuffle=function(obj){var shuffled=[],rand;return each(obj,function(value,index,list){if(0==index)shuffled[0]=value;else rand=Math.floor(Math.random()*(index+1)),shuffled[index]=shuffled[rand],shuffled[rand]=value}),shuffled},_.sortBy=function(obj,iterator,context){return _.pluck(_.map(obj,function(value,index,list){return{value:value,criteria:iterator.call(context,value,index,list)}}).sort(function(left,right){var a=left.criteria,b=right.criteria;return b>a?-1:a>b?1:0}),"value")},_.groupBy=function(obj,val){var result={},iterator=_.isFunction(val)?val:function(obj){return obj[val]};return each(obj,function(value,index){var key=iterator(value,index);(result[key]||(result[key]=[])).push(value)}),result},_.sortedIndex=function(array,obj,iterator){iterator||(iterator=_.identity);for(var low=0,high=array.length;high>low;){var mid=low+high>>1;iterator(array[mid])<iterator(obj)?low=mid+1:high=mid}return low},_.toArray=function(iterable){if(!iterable)return[];if(iterable.toArray)return iterable.toArray();if(_.isArray(iterable))return slice.call(iterable);if(_.isArguments(iterable))return slice.call(iterable);return _.values(iterable)},_.size=function(obj){return _.toArray(obj).length},_.first=_.head=function(array,n,guard){return null!=n&&!guard?slice.call(array,0,n):array[0]},_.initial=function(array,n,guard){return slice.call(array,0,array.length-(null==n||guard?1:n))},_.last=function(array,n,guard){if(null!=n&&!guard)return slice.call(array,Math.max(array.length-n,0));else return array[array.length-1]},_.rest=_.tail=function(array,index,guard){return slice.call(array,null==index||guard?1:index)},_.compact=function(array){return _.filter(array,function(value){return!!value})},_.flatten=function(array,shallow){return _.reduce(array,function(memo,value){if(_.isArray(value))return memo.concat(shallow?value:_.flatten(value));return memo[memo.length]=value,memo},[])},_.without=function(array){return _.difference(array,slice.call(arguments,1))},_.uniq=_.unique=function(array,isSorted,iterator){var initial=iterator?_.map(array,iterator):array,result=[];return _.reduce(initial,function(memo,el,i){if(0==i||(isSorted===!0?_.last(memo)!=el:!_.include(memo,el)))memo[memo.length]=el,result[result.length]=array[i];return memo},[]),result},_.union=function(){return _.uniq(_.flatten(arguments,!0))},_.intersection=_.intersect=function(array){var rest=slice.call(arguments,1);return _.filter(_.uniq(array),function(item){return _.every(rest,function(other){return _.indexOf(other,item)>=0})})},_.difference=function(array){var rest=_.flatten(slice.call(arguments,1));return _.filter(array,function(value){return!_.include(rest,value)})},_.zip=function(){for(var args=slice.call(arguments),length=_.max(_.pluck(args,"length")),results=new Array(length),i=0;length>i;i++)results[i]=_.pluck(args,""+i);return results},_.indexOf=function(array,item,isSorted){if(null==array)return-1;var i,l;if(isSorted)return i=_.sortedIndex(array,item),array[i]===item?i:-1;if(nativeIndexOf&&array.indexOf===nativeIndexOf)return array.indexOf(item);for(i=0,l=array.length;l>i;i++)if(i in array&&array[i]===item)return i;return-1},_.lastIndexOf=function(array,item){if(null==array)return-1;if(nativeLastIndexOf&&array.lastIndexOf===nativeLastIndexOf)return array.lastIndexOf(item);for(var i=array.length;i--;)if(i in array&&array[i]===item)return i;return-1},_.range=function(start,stop,step){if(arguments.length<=1)stop=start||0,start=0;step=arguments[2]||1;for(var len=Math.max(Math.ceil((stop-start)/step),0),idx=0,range=new Array(len);len>idx;)range[idx++]=start,start+=step;return range};var ctor=function(){};if(_.bind=function bind(func,context){var bound,args;if(func.bind===nativeBind&&nativeBind)return nativeBind.apply(func,slice.call(arguments,1));if(!_.isFunction(func))throw new TypeError;return args=slice.call(arguments,2),bound=function(){if(!(this instanceof bound))return func.apply(context,args.concat(slice.call(arguments)));ctor.prototype=func.prototype;var self=new ctor,result=func.apply(self,args.concat(slice.call(arguments)));if(Object(result)===result)return result;return self}},_.bindAll=function(obj){var funcs=slice.call(arguments,1);if(0==funcs.length)funcs=_.functions(obj);return each(funcs,function(f){obj[f]=_.bind(obj[f],obj)}),obj},_.memoize=function(func,hasher){var memo={};return hasher||(hasher=_.identity),function(){var key=hasher.apply(this,arguments);return _.has(memo,key)?memo[key]:memo[key]=func.apply(this,arguments)}},_.delay=function(func,wait){var args=slice.call(arguments,2);return setTimeout(function(){return func.apply(func,args)},wait)},_.defer=function(func){return _.delay.apply(_,[func,1].concat(slice.call(arguments,1)))},_.throttle=function(func,wait){var context,args,timeout,throttling,more,whenDone=_.debounce(function(){more=throttling=!1},wait);return function(){context=this,args=arguments;var later=function(){if(timeout=null,more)func.apply(context,args);whenDone()};if(!timeout)timeout=setTimeout(later,wait);if(throttling)more=!0;else func.apply(context,args);whenDone(),throttling=!0}},_.debounce=function(func,wait){var timeout;return function(){var context=this,args=arguments,later=function(){timeout=null,func.apply(context,args)};clearTimeout(timeout),timeout=setTimeout(later,wait)}},_.once=function(func){var ran=!1,memo;return function(){if(ran)return memo;return ran=!0,memo=func.apply(this,arguments)}},_.wrap=function(func,wrapper){return function(){var args=[func].concat(slice.call(arguments,0));return wrapper.apply(this,args)}},_.compose=function(){var funcs=arguments;return function(){for(var args=arguments,i=funcs.length-1;i>=0;i--)args=[funcs[i].apply(this,args)];return args[0]}},_.after=function(times,func){if(0>=times)return func();return function(){if(--times<1)return func.apply(this,arguments)}},_.keys=nativeKeys||function(obj){if(obj!==Object(obj))throw new TypeError("Invalid object");var keys=[];for(var key in obj)if(_.has(obj,key))keys[keys.length]=key;return keys},_.values=function(obj){return _.map(obj,_.identity)},_.functions=_.methods=function(obj){var names=[];for(var key in obj)if(_.isFunction(obj[key]))names.push(key);return names.sort()},_.extend=function(obj){return each(slice.call(arguments,1),function(source){for(var prop in source)obj[prop]=source[prop]}),obj},_.defaults=function(obj){return each(slice.call(arguments,1),function(source){for(var prop in source)if(null==obj[prop])obj[prop]=source[prop]}),obj},_.clone=function(obj){if(!_.isObject(obj))return obj;return _.isArray(obj)?obj.slice():_.extend({},obj)},_.tap=function(obj,interceptor){return interceptor(obj),obj},_.isEqual=function(a,b){return eq(a,b,[])},_.isEmpty=function(obj){if(_.isArray(obj)||_.isString(obj))return 0===obj.length;for(var key in obj)if(_.has(obj,key))return!1;return!0},_.isElement=function(obj){return!(!obj||1!=obj.nodeType)},_.isArray=nativeIsArray||function(obj){return"[object Array]"==toString.call(obj)},_.isObject=function(obj){return obj===Object(obj)},_.isArguments=function(obj){return"[object Arguments]"==toString.call(obj)},!_.isArguments(arguments))_.isArguments=function(obj){return!(!obj||!_.has(obj,"callee"))};_.isFunction=function(obj){return"[object Function]"==toString.call(obj)},_.isString=function(obj){return"[object String]"==toString.call(obj)},_.isNumber=function(obj){return"[object Number]"==toString.call(obj)},_.isNaN=function(obj){return obj!==obj},_.isBoolean=function(obj){return obj===!0||obj===!1||"[object Boolean]"==toString.call(obj)},_.isDate=function(obj){return"[object Date]"==toString.call(obj)},_.isRegExp=function(obj){return"[object RegExp]"==toString.call(obj)},_.isNull=function(obj){return null===obj},_.isUndefined=function(obj){return void 0===obj},_.has=function(obj,key){return hasOwnProperty.call(obj,key)},_.noConflict=function(){return root._=previousUnderscore,this},_.identity=function(value){return value},_.times=function(n,iterator,context){for(var i=0;n>i;i++)iterator.call(context,i)},_.escape=function(string){return(""+string).replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#x27;").replace(/\//g,"&#x2F;")},_.mixin=function(obj){each(_.functions(obj),function(name){addToWrapper(name,_[name]=obj[name])})};var idCounter=0;_.uniqueId=function(prefix){var id=idCounter++;return prefix?prefix+id:id},_.templateSettings={evaluate:/<%([\s\S]+?)%>/g,interpolate:/<%=([\s\S]+?)%>/g,escape:/<%-([\s\S]+?)%>/g};var noMatch=/.^/,unescape=function(code){return code.replace(/\\\\/g,"\\").replace(/\\'/g,"'")};_.template=function(str,data){var c=_.templateSettings,tmpl="var __p=[],print=function(){__p.push.apply(__p,arguments);};with(obj||{}){__p.push('"+str.replace(/\\/g,"\\\\").replace(/'/g,"\\'").replace(c.escape||noMatch,function(match,code){return"',_.escape("+unescape(code)+"),'"}).replace(c.interpolate||noMatch,function(match,code){return"',"+unescape(code)+",'"}).replace(c.evaluate||noMatch,function(match,code){return"');"+unescape(code).replace(/[\r\n\t]/g," ")+";__p.push('"}).replace(/\r/g,"\\r").replace(/\n/g,"\\n").replace(/\t/g,"\\t")+"');}return __p.join('');",func=new Function("obj","_",tmpl);if(data)return func(data,_);return function(data){return func.call(this,data,_)}},_.chain=function(obj){return _(obj).chain()};var wrapper=function(obj){this._wrapped=obj};_.prototype=wrapper.prototype;var result=function(obj,chain){return chain?_(obj).chain():obj},addToWrapper=function(name,func){wrapper.prototype[name]=function(){var args=slice.call(arguments);return unshift.call(args,this._wrapped),result(func.apply(_,args),this._chain)}};_.mixin(_),each(["pop","push","reverse","shift","sort","splice","unshift"],function(name){var method=ArrayProto[name];wrapper.prototype[name]=function(){var wrapped=this._wrapped;method.apply(wrapped,arguments);var length=wrapped.length;if(("shift"==name||"splice"==name)&&0===length)delete wrapped[0];return result(wrapped,this._chain)}}),each(["concat","join","slice"],function(name){var method=ArrayProto[name];wrapper.prototype[name]=function(){return result(method.apply(this._wrapped,arguments),this._chain)}}),wrapper.prototype.chain=function(){return this._chain=!0,this},wrapper.prototype.value=function(){return this._wrapped}}).call(this);