var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber');
var watch = require('gulp-watch');
var del = require('del');

// builds elm files and static resources (i.e. html and css)
// from src to dist folder
var paths = {
  dest: 'dist',
  elm: 'src/*.elm',
  staticAssets: 'src/*.{html,css}'
};

// clean
gulp.task('clean', function(cb) {
  del([paths.dest], cb);
});

gulp.task('elm', function(){
  return gulp.src(paths.elm, { optimize: true })
    .pipe(plumber())
    .pipe(elm({filetype : 'html'}))
    .pipe(gulp.dest(paths.dest));
});

gulp.task('staticAssets', function() {
  return gulp.src(paths.staticAssets)
    .pipe(plumber())
    .pipe(gulp.dest(paths.dest));
});

gulp.task('watch', function() {
  gulp.watch(paths.elm, gulp.series('build'));
});
  

gulp.task('build', gulp.series('elm', 'staticAssets'));
gulp.task('dev', gulp.series('build', 'watch'));
gulp.task('default', gulp.series('build'));