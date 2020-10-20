from django.db import models
import uuid

# Create your models here.
class PostCategory (models.Model):
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    category_name = models.CharField(max_length=32, null=False, blank=False)
    description = models.TextField(null=True, blank=True)
    created = models.DateTimeField(auto_now_add=True)


class Serie (models.Model):
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    serie_name = models.CharField(max_length=32, null=False, blank=False)
    description = models.TextField(null=True, default=None)
    created = models.DateTimeField(auto_now_add=True)

class Post(models.Model):
    id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
    post = models.TextField(null=False, blank=False)
    categories = models.ManyToManyField(PostCategory, blank=False)
    serie = models.ForeignKey(Serie, on_delete=models.CASCADE, null=True, blank=True)
    published = models.BooleanField(default=False, blank=True)
    created = models.DateTimeField(auto_now_add=True)
    updated = models.DateTimeField(auto_now=True)


