  function startup(){
    if (document.documentElement.clientWidth < 800) {
  	  showvelo();
    }
  }
  
  function openNav(){
    document.getElementById("menu").style.width = "100%";
  }
  
  function closeNav(){
    document.getElementById("menu").style.width = "0%";
  }
  
  
  
  function changevisual(){
    var a = document.getElementById("newvisual").value;
    if(a == 1){
      showmovement();
    }
    if(a == 2){
      showvelo();
    }
    if(a == 3){
      showloclhb();
    }
    if(a == 4){
      showlocrhb();
    }
    if(a == 5){
      showrelease();
    }
    if(a == 6){
      showextension();
    }
    if(a == 7){
      showspin();
    }
    if(a == 8){
      showstats1();
    }
    if(a == 9){
      showstats2();
    }
    if(a == 10){
      showstats3();
    }
    if(a == 11){
      showbattedballtype();
    }
  }
  
  
  function hideeverything(){
    var a = document.getElementById("stats1");
    var a2 = document.getElementById("stats2");
    var a3 = document.getElementById("stats3");
    var b = document.getElementById("movement");
    var c = document.getElementById("velo");
    var d1 = document.getElementById("loclhb");
    var d2 = document.getElementById("locrhb");
    var x = document.getElementById("release");
    var y = document.getElementById("extension");
    var z = document.getElementById("spin");
    var u = document.getElementById("battedballtype");
    
    document.getElementById("locslegend").style.display = "none";
    
    a.style.display = "none";
    a2.style.display = "none";
	  a3.style.display = "none";    
    b.style.display = "none";
    c.style.display = "none";
    d1.style.display = "none";
    d2.style.display = "none";
    x.style.display = "none";
    y.style.display = "none";
    z.style.display = "none";
    u.style.display = "none";
  }

  function showstats1(){
    hideeverything();
    var a = document.getElementById("stats1");
    a.style.display = "block";
  }
  
  function showstats2(){
    hideeverything();
    var a2 = document.getElementById("stats2");
    a2.style.display = "block";
    
  }
  
  function showstats3(){
    hideeverything();
    var a3 = document.getElementById("stats3");
    a3.style.display = "block";
  }
  
  function showmovement(){
    hideeverything();
    var b = document.getElementById("movement");
    b.style.display = "block";
    
  }
  
  
  function showvelo(){
    hideeverything();
    var c = document.getElementById("velo");
    c.style.display = "block";
  }  
  
  function showloclhb(){
    hideeverything();
    var d1 = document.getElementById("loclhb");
    d1.style.display = "block";
    document.getElementById("locslegend").style.display = "block";
  }  
  
  function showlocrhb(){
    hideeverything();
    var d2 = document.getElementById("locrhb");
    d2.style.display = "block";
    document.getElementById("locslegend").style.display = "block";
  }  
  
  
  function showrelease(){
    hideeverything();
    var x = document.getElementById("release");
    x.style.display = "block";
    
  }  
  
  function showextension(){
    hideeverything();
    var y = document.getElementById("extension");
    y.style.display = "block";

  }  
  
  function showspin(){
    hideeverything();
    var z = document.getElementById("spin");
    z.style.display = "block";
    
  }  
  
  
  function showbattedballtype(){
    hideeverything();
    var u = document.getElementById("battedballtype");
    u.style.display = "block";
    
  }  
  
  

  function showavgsbtns(){
    var a = document.getElementById("avgsbtns");
    if(a.style.display == "none" | a.classList.contains('noshow2start')){
      a.style.display = "block";
      a.classList.remove('noshow2start');
    } else{
      a.style.display = "none";
    }
    
  }