// Adapted from Javascript Garden, a MIT project.
function Sections(page) {
    this.page = page;
    this.init();
}

Sections.prototype = {
    init: function(attribute) {
        this.heights = this.page.nav.find('ul').map(function(idx, ele) {
            return $(this).outerHeight();
        }).get();
    },

    map: function() {
        this.names = $('h2').map(function(idx, ele) {
            return {
                id: this.id,
                offset: $(this).offset().top + 30,
                title: $(this).find(':header:first').html()
            };
        }).get();
    },

    highlight: function() {

        var scroll = this.page.window.scrollTop(),
            articleID = this.names[this.names.length - 1].id;

        $('a').removeClass('active');

        var $el;

        for(var i = 0, l = this.names.length; i < l; i++) {
            if (this.names[i].offset > scroll) {
                $el = $("[href='#" + this.names[i].id + "']");
                var s = $el.parents('ul')[0];

                // $el.addClass('active');

                if (s !== window.section) {
                    //$(window.section).slideUp();
                    $(window.section).hide();
                    //$(s).slideDown();
                    $(s).show();
                    window.section = s;
                }

                break;
            }
        }
    },

    updateLinks: function(index) {
        if (index !== this.names.length - 1) {
            this.setLink(this.links.next, this.names[index + 1]);
        } else {
            //this.links.next.slideUp(100);
            this.links.next.hide();
        }

        if (index !== 0) {
            this.setLink(this.links.prev, this.names[index - 1]);
        } else {
            //this.links.prev.slideUp(100);
            this.links.next.hide();
        }
    },

    setLink: function(ele, data) {
        ele.slideDown(100).attr('href', '#' + data.id)
           .find('.nav_section_name').html(data.title);
    }
};

function Page() {
    $.extend(true, this, {
        window: $(window),
        nav: $('.nav > ul > li'),
        section: null,
        articule: null
    });
    
    this.sections = new Sections(this);
    this.init();
}

Page.prototype = {
    init: function() {
        var that = this,
            mainNav = $('.toc');

        $.extend(this, {
            scrollLast: 0,
            resizeTimeout: null
        });
        
        this.window.scroll(function() {
            that.onScroll();
        });
        
        this.window.resize(function() {
            that.onResize();
        });

        that.sections.map();
        setTimeout(function() {
            that.sections.highlight();
        }, 10);
    },

    onScroll: function() {
        if ((+new Date()) - this.scrollLast > 50) {
            this.scrollLast = +new Date();
            this.sections.highlight();
        }
    },

    onResize: function() {
        clearTimeout(this.resizeTimeout);
    }
};

$(document).ready(function() {
    if ($(window).width() > 481) {
      var page = new Page();
      page.scrolllast = new Date();
    }

    //$('.side ul ul').hide();
    //$('.side ul ul').first().show();
});
