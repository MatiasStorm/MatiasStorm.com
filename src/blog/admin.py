from django.apps import apps
from django.contrib import admin
from . import models

# Register your models here.
admin.site.register(models.PostCategory)
admin.site.register(models.Serie)
admin.site.register(models.Post)
